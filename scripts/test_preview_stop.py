"""Windows regression tests for the task-owned Quarto preview shutdown."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time
import unittest


@unittest.skipUnless(os.name == "nt", "preview process ownership is Windows-specific")
class PreviewStopTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(dir=Path(__file__).parent)
        self.root = Path(self.temporary.name)
        self.script = self.root / "scripts" / "site.ps1"
        self.script.parent.mkdir()
        shutil.copy2(Path(__file__).with_name("site.ps1"), self.script)
        self.state = self.root / ".codex" / "state" / "preview.json"
        self.state.parent.mkdir(parents=True)
        self.powershell = shutil.which("powershell")
        if not self.powershell:
            self.skipTest("Windows PowerShell is unavailable")
        self.child = None

    def tearDown(self):
        if self.child and self.child.poll() is None:
            self.child.terminate()
            try:
                self.child.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.child.kill()
                self.child.wait(timeout=5)
        self.temporary.cleanup()

    def start_harmless_process(self):
        self.child = subprocess.Popen(
            [self.powershell, "-NoProfile", "-Command", "Start-Sleep -Seconds 60"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            result = subprocess.run(
                [
                    self.powershell,
                    "-NoProfile",
                    "-Command",
                    f"(Get-Process -Id {self.child.pid}).StartTime.ToUniversalTime().ToString('o')",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            started = result.stdout.strip()
            if result.returncode == 0 and started:
                return started
            time.sleep(0.05)
        self.fail("the harmless child process did not expose a start time")

    def stop_preview(self):
        return subprocess.run(
            [
                self.powershell,
                "-NoProfile",
                "-ExecutionPolicy",
                "Bypass",
                "-File",
                str(self.script),
                "-Action",
                "stop",
                "-Python",
                sys.executable,
            ],
            cwd=self.root,
            capture_output=True,
            text=True,
            check=False,
        )

    def write_state(self, started):
        self.state.write_text(
            json.dumps({"proxyPid": self.child.pid, "proxyStarted": started}),
            encoding="utf-8",
        )

    def test_matching_timestamp_stops_only_the_owned_process(self):
        self.write_state(self.start_harmless_process())

        result = self.stop_preview()

        self.assertEqual(result.returncode, 0, result.stderr)
        self.child.wait(timeout=5)
        self.assertIsNotNone(self.child.returncode)
        self.assertFalse(self.state.exists())

    def test_mismatched_timestamp_preserves_a_reused_process_id(self):
        self.start_harmless_process()
        self.write_state("2000-01-01T00:00:00.0000000Z")

        result = self.stop_preview()

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIsNone(self.child.poll())
        self.assertFalse(self.state.exists())


if __name__ == "__main__":
    unittest.main()
