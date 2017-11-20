# Obras-PB  

Uma plataforma que auxilie gestores, auditores e cidadãos no acompanhamento e controle do bom uso dos recursos públicos nas obras realizadas na Paraíba.

## Como usar

Para replicar e rodar esta aplicação será necessário ter instalado em seu computador o [Git](https://git-scm.com), [Java](https://www.java.com), [Scala](https://www.scala-lang.org/) e [SBT](http://www.scala-sbt.org/). Para rodar a aplicação execute os seguintes comandos:

```bash
# Clone este repositório
$ git clone https://github.com/analytics-ufcg/obras-pb.git

# Copie o arquivo de configuração para a pasta adequada
$ cp <caminho-para-o-arquivo>/application.conf obras-pb/conf/application.conf

# Entre no repositório
$ cd obras-pb

# Rode a aplicação
$ sbt run

# =============== Modo de produção ===============
# Alternativamente, para rodar a aplicação em modo de produção
$ sbt dist

# Mova o arquivo para a pasta desejada
$ mv target/universal/obras-pb-1.0.zip <pasta-desejava>

# Extraia o arquivo
$ unzip obras-pb-1.0.zip

# Entre na pasta que contém o arquivo executável
$ cd obras-pb-1.0/bin/

# Execute o arquivo na porta desejava
$ ./obras-pb -Dhttp.port=<porta>
```

**NOTA**: Para rodar a aplicação com sucesso é necessário ter o arquivo `application.conf`, o qual contém acesso ao banco de dados do TCE. 
