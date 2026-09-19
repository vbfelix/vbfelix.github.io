---
name: vfx-criar-commit
description: Use when changes are ready to record in history; not for sending them anywhere, which stays with publicar-alteracoes.
---

# Criar commit

Trate alterações preexistentes como pertencentes ao usuário: confira o estado do repositório antes de qualquer staging e inclua somente o que pertence a esta alteração.

Faça staging por arquivo ou trecho, revise o diff staged e execute `verificar-alteracao` antes de commitar. A mensagem descreve o efeito da mudança e o motivo dela; a lista de arquivos já está no diff.

Não use stash, reset, clean nem mudança de upstream para contornar um staging difícil: eles descartam trabalho do usuário.

Commit registra história local. Push, PR, merge e publicação dependem de `publicar-alteracoes` a pedido explícito do usuário.
