# Obras-PB  

Visão das obras realizadas na PB e cadastradas no TCE/PB.

## Rodando

Você precisará de [Java](https://www.java.com), [Scala](https://www.scala-lang.org/) e [SBT](http://www.scala-sbt.org/). 

Para rodar a aplicação:

```bash
$ git clone https://github.com/analytics-ufcg/obras-pb.git

# Copie um arquivo de configuração especificando acesso ao BD
$ cp <caminho-para-o-arquivo>/application.conf obras-pb/conf/application.conf

$ cd obras-pb
$ sbt run

# =============== Modo de produção ===============
# Para rodar em modo de produção
$ sbt dist
$ mv target/universal/obras-pb-1.0.zip <pasta-desejava>
$ unzip obras-pb-1.0.zip
$ obras-pb-1.0/bin/obras-pb -Dhttp.port=<porta>
```

**NOTA**: Para rodar a aplicação é necessário um arquivo `application.conf` configurando acesso a uma imagem do BD do Sagres do TCE/PB. 
