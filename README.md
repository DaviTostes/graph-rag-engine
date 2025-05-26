# Haskell Text Embedding RAG com Grafo

Este projeto em Haskell implementa um pipeline completo de **Geração Aumentada por Recuperação (RAG)**, utilizando embeddings de texto e um grafo baseado em similaridade de cosseno. Ele permite:

- Processar arquivos `.txt` de um diretório (`assets/chunks`).
- Gerar embeddings com um modelo local (`nomic-embed-text` via Ollama em `localhost:11434`).
- Construir um grafo de similaridade entre os textos.
- Realizar buscas RAG com base nesse grafo, retornando trechos de texto relevantes.

## Estrutura

- `assets/chunks/`: coloque aqui os arquivos `.txt` para processamento.
- `Context.hs`: leitura dos arquivos, geração de embeddings, criação e salvamento dos contextos.
- `Main.hs`: leitura dos contextos, criação do grafo e execução da busca.

## Requisitos

- GHC + Stack
- API local do modelo de embedding (`nomic-embed-text`) escutando em `localhost:11434`
- Pasta `assets/chunks/` com arquivos `.txt`

## Instalação

```bash
git clone https://github.com/seu-usuario/haskell-rag-grafo.git
cd haskell-rag-grafo
stack setup
stack build
```

## Como usar
### 1. Gerar os contextos e embeddings

Execute em um REPL (stack ghci) ou crie um binário que chama:

```haskell
createContextsFile
```

Isso vai:

- Ler os .txt de assets/chunks/
- Gerar embeddings via POST para localhost:11434/api/embed
- Salvar os contextos com vetores em assets/contexts.json

### 2. Rodar a busca RAG no grafo

```bash
stack run -- "sua pergunta ou texto aqui"
```
 
A aplicação irá:

- Carregar os contextos de assets/contexts.json
- Gerar embedding da entrada
- Calcular similaridade com os nós
- Navegar no grafo e concatenar textos similares

Exemplo:

```bash
stack run -- "O que é energia solar?"
```

Saída (exemplo):

```txt
[Texto mais similar do grafo]
[Outro texto conectado com alta similaridade]
```

### 3. Gerar representação textual do grafo

Dentro de Main.hs, a função createGraphFile salva o grafo em assets/graph.txt, útil para análise ou debug.

## Lógica de Similaridade

- Similaridade de cosseno é usada para criar arestas entre vetores.
- Um limiar (edgeTreshold = 0.85) define quando uma aresta é criada.
- Outro limiar (queryThreshold = 0.7) define se um nó é considerado relevante para a consulta.

## Exemplo de estrutura JSON gerada

```json
[
  {
    "cId": 0,
    "title": "exemplo.txt",
    "text": "conteúdo do texto",
    "vector": [0.12, -0.87, ...]
  },
  ...
]
```

## Licença

```txt
MIT – Sinta-se livre para usar, modificar e distribuir.
```

Se quiser, posso gerar o [arquivo pronto para download](f) ou um [diagrama explicativo do fluxo](f).
