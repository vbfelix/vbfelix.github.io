# O maior datalake da pecuária brasileira

Fonte: https://vbfelix.github.io/portfolio/0035-datalake-pecuaria/index.html

![Servidores de duas fazendas enviam dados para uma base na nuvem e uma tabela comum. Ilustração conceitual da integração e padronização de sistemas locais.](https://vbfelix.github.io/portfolio/0035-datalake-pecuaria/thumbnail.svg)

Em 2022, eu vi Windows 95 rodando em uma fazenda. E nosso desafio era levar os dados daquele universo para a nuvem.

Eu trabalhava com o maior sistema de confinamento do Brasil, em uma empresa líder de mercado. O sistema era desktop e funcionava offline, com infraestrutura própria em cada fazenda. Queríamos integrar dezenas de bases em um datalake online, um ambiente central para reunir dados de diferentes origens e permitir seu consumo.

Participar dessa construção significava lidar com versões diferentes do sistema, falta de internet e interrupções de energia. Tudo isso sem comprometer a operação das fazendas, que dependia do sistema funcionando.

# A nuvem começava no servidor da fazenda

Cada fazenda tinha seu servidor. Encontrar Windows 95 em 2022 dava uma dimensão da variedade de ambientes com que precisávamos lidar.

O desafio era criar um conector, um programa que permitisse retirar os dados do sistema local e enviá-los ao ambiente central. Ele precisava funcionar em infraestruturas muito diferentes e consumir poucos recursos. Poder computacional e conexão eram restrições reais.

A orientação era subir somente o necessário e reconstruir na nuvem o que fosse possível. O trabalho de integração precisava caber na infraestrutura disponível, respeitando a prioridade da operação local.

Nas minhas [notas sobre data warehouse](https://vbfelix.github.io/posts/0024-dw/index.html), um ambiente de dados preparado para análise e relatórios, discuto a importância de separar esse uso dos sistemas que registram a operação cotidiana. Essa preocupação ajuda a entender o desafio aqui: o sistema da fazenda precisava continuar atendendo à fazenda enquanto construíamos outra forma de consumir seus dados.

# O cadastro também precisava conversar

O sistema tinha muitos campos abertos, principalmente nos cadastros de produtos. Reunir esses registros trazia outro trabalho: normalizar e padronizar o que cada fazenda preenchia.

A centralização exigia lidar tanto com a transferência dos dados quanto com o significado dos registros. E os cadastros eram apenas uma parte dessa dificuldade.

# A mesma coluna, medidas diferentes

O sistema possuía dezenas de parâmetros. Como produto, essa flexibilidade era incrível. Para analisar os dados de várias fazendas juntas, a quantidade de combinações era um pesadelo.

Algumas diferenças explicam o tamanho do problema:

- **O peso podia ser individual ou médio.** Algumas fazendas pesavam cada animal; outras obtinham o peso médio pela pesagem do caminhão. Era preciso considerar como a medida havia sido produzida antes de compará-la.
- **O estoque podia vir da balança ou da nota fiscal.** Reunir os valores exigia reconhecer a diferença entre essas formas de registro.
- **A coleta podia depender de sensores ou de dados autodeclarados.** A origem da informação fazia parte do problema de qualidade.
- **O manejo variava muito.** Havia pastagem, suplementação, dietas intensivas e ingredientes variados. Essa diversidade precisava entrar na discussão sobre quais fazendas comparar.

Fazer benchmarking, isto é, usar os resultados de outras fazendas como referência de desempenho, exigia cuidado. Até uma média podia induzir a uma interpretação errada quando reunia realidades diferentes.

# Quando Simpson entrou na fazenda

Eu já conhecia o Paradoxo de Simpson na teoria. Foi ao juntar dados de diferentes fazendas, em uma análise de eficiência operacional e financeira, que o vi aparecer com tanta nitidez na prática.

O paradoxo ocorre quando a direção de uma relação observada dentro dos grupos se inverte ao reunir os dados. No meu [artigo sobre o Paradoxo de Simpson](https://vbfelix.github.io/posts/0014-simpson-paradox/index.html), apresento um exemplo didático: a relação é positiva dentro de cada grupo e negativa no conjunto. Olhar apenas o resultado agregado muda a interpretação.

Na análise das fazendas, selecionar o que entrava e decidir o que filtrar era um desafio central. Precisávamos buscar uma amostra representativa sem induzir a conclusões erradas. Entender as diferenças de manejo e de medição fazia parte desse trabalho, assim como conhecer os dados que chegavam de cada sistema.

# Aprendizados, lições, erros e principais impactos

Montamos um ambiente central de consumo de dados a partir das bases offline das fazendas. Na minha participação nesse trabalho, três aprendizados se destacaram:

- **A infraestrutura da fazenda precisava orientar a arquitetura.** A operação dependia de servidores com recursos limitados e conexão instável. Enviar somente o necessário e reconstruir o possível na nuvem era a orientação para respeitar essas restrições.
- **Padronizar cadastros era apenas parte da qualidade.** O peso individual e o peso médio obtido pelo caminhão mostravam que também era preciso entender como cada medida havia sido produzida para decidir o que comparar.
- **A análise agregada podia inverter a interpretação.** Ver o Paradoxo de Simpson na análise de eficiência operacional e financeira tornou concreta uma armadilha que eu conhecia da teoria: tirar conclusões sobre as fazendas apenas pelo comportamento do conjunto. Selecionar e filtrar os dados exigia tanto cuidado quanto reuni-los.

A centralização criou um ponto comum de consumo. Construir comparações representativas continuava sendo um desafio próprio, dependente da qualidade dos registros e do entendimento de cada operação.
