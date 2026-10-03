---
name: vfx-create-commit
description: Use when changes are ready to be recorded in a local commit or the user asks to commit. Not for pushing to the remote, which is `publish-changes`.
---

# Criar commit

Trate alterações preexistentes como pertencentes ao usuário: confira o estado do repositório antes de qualquer staging e inclua somente o que pertence a esta alteração.

Faça staging por arquivo ou trecho, revise o diff staged e execute `verify-change` antes de commitar. A mensagem descreve o efeito da mudança e o motivo dela; a lista de arquivos já está no diff.

Não use stash, reset, clean nem mudança de upstream para contornar um staging difícil: eles descartam trabalho do usuário.

Commit registra história local. Push, PR, merge e publicação dependem de `publish-changes` a pedido explícito do usuário.
