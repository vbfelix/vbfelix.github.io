"""Forward the local Quarto preview with explicit UTF-8 text responses."""
import argparse
import http.client
import select
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


class Preview(BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'

    def do_GET(self):
        connection = http.client.HTTPConnection('127.0.0.1', self.server.backend, timeout=30)
        try:
            connection.request('GET', self.path, headers=dict(self.headers))
            response = connection.getresponse()
            self.send_response(response.status)
            upgrade = response.status == 101
            for name, value in response.getheaders():
                if name.lower() in ('transfer-encoding', 'connection') and not upgrade:
                    continue
                if name.lower() == 'content-type' and value.split(';')[0] in ('text/markdown', 'text/plain'):
                    # Browsers display raw Markdown reliably as UTF-8 plain text.
                    value = 'text/plain; charset=utf-8'
                self.send_header(name, value)
            if not upgrade:
                self.send_header('Connection', 'close')
            self.end_headers()
            if upgrade:
                upstream = connection.sock
                while upstream:
                    readable, _, _ = select.select([self.connection, upstream], [], [], 60)
                    for source in readable:
                        data = source.recv(65536)
                        if not data:
                            return
                        (upstream if source is self.connection else self.connection).sendall(data)
            else:
                self.wfile.write(response.read())
        except (ConnectionError, TimeoutError, OSError):
            self.close_connection = True
        finally:
            connection.close()
            self.close_connection = True


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', type=int, default=4321)
    parser.add_argument('--backend', type=int, default=4323)
    args = parser.parse_args()
    server = ThreadingHTTPServer(('127.0.0.1', args.port), Preview)
    server.backend = args.backend
    server.serve_forever()
