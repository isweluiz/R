veiculos <- read.csv2("C:/Users/1831133001/Downloads/dados_veiculos.csv", header=TRUE, dec="," , fileEncoding = "UTF-8")
veiculos

modelo <- veiculos['TipoVeiculo']
ano_carros <- veiculos['ano'] 

log_caro <- log(ano_carros)
freq_ano <- frequency(ano_carros)
mean(ano_carros)
freq_modelo <- frequency(modelo)
x <- ano_carros
str(veiculos) #Tipo de cada coluna do meu dataframe
#Qual a distribuição de frequências do ano?
tab <- aggregate(IdAnuncio ~ Marca + Modelo + ano, FUN='length', data=veiculos)
preco = veiculos['vehiclePrice']

summary(preco)
print(min(preco))

#Qual o modelo mais frequente?
#Transformando int em string
veiculos$Modelo <- as.character(veiculos$Modelo)
media = data.frame(table(veiculos$vehiclePrice))
#Lista de modelos, unique > Utilizado para n repetir o mesmo valor
lista_modelo <- unique(veiculos$Modelo)
m <- "C3"
print(mean(veiculos$vehiclePrice[veiculos$Modelo==m]))

#Com o comando for, imprima a média e o desvio-padrão dos preços por modelo
for (i in lista_modelo){
  print(i)
  print(mean(veiculos$vehiclePrice[veiculos$Modelo==m]))
  print(min(veiculos$vehiclePrice[veiculos$Modelo==m]))
  print(max(veiculos$vehiclePrice[veiculos$Modelo==m]))
}
#Qual a marca com a maior média de preços?
print(mean(veiculos$vehiclePrice[veiculos$Modelo==m]))
marca_veiculo <- unique(veiculos$Marca)
#print(mean(veiculos$marca_veiculo[veiculos$vehiclePrice]))

#Qual a média de km dos carros de 2013?
#Crie um campo “idade” que representa a idade dos veículos (faça 2018 – ano)
#Se essa amostra de veículos representasse toda a população de veículos do Brasil, qual seria sua estimativa para a média de km percorridos por ano pelo brasileiro?
#Qual é o anúncio mais barato na amostra? Use a função which.min()
#Faça um histograma da quilometragem

veiculos$idade <- 2018 - veiculos$ano
veiculos$km_ano <-  veiculos$km/veiculos$idade
hist(veiculos$km_ano)
plot(veiculos$km_ano,type ="S", sub = "Aula de R", col ="black" ,main = "Média de KM dos carros de 2018", ylab=" ", xlab="Kilometragem") #Grafico de linha
media_km_ano <- mean(veiculos$km_ano[veiculos$ano < 2018], na.rm = T)
veiculos[which.min(veiculos$vehiclePrice),] #Marca com o menor preço 
veiculos[which.max(veiculos$vehiclePrice),] #Marca com o maior preço 
veiculos["Origem"]
hist(veiculos$km, main = "Quantidade de km por veículo", col = "green", xlab = "Kilometragem", border ='0', )
plot(veiculos$km, type = "o", main = "Km por veiculo", ylab = "Número de veiculos", xlab = "Kilometragem")
#Como podemos identificar anúncios com quilometragem ou preços errados?

#Construa a função SOMA que recebe como parâmetro dois vetores numéricos e:
#Se os lengths dos dois vetores são iguais, retorna a soma dos elementos do primeiro vetor dividido pela soma dos elementos do segundo
#Caso contrário, imprime a mensagem “Erro: tamanhos distintos”

funcaoSoma <- function(x,y){
  if(length(x)==length(y)){ #Comparando tamanho e igualdade entre variaveis
    sum(x)/sum(y) 
  }else{
    print("Erro: tamanhos distintos")
  }
}
#criando vetores
vetor01 <- c(1,2,3,4)
vetor02 <- c(5,6,7,8)
vetor03 <- c(9,10,11,12)
funcaoSoma(vetor01, vetor02)
funcaoSoma(x=vetor01, y=vetor03)
funcaoSoma()
nrow(veiculos) #Número de linhas
dim(veiculos) # Número de linhas & colunas
# Ordenação de objetos(registros)
arrange(veiculos)
#Seleciona colunas 
select(coluna1, coluna2)
#Apaga coluna
select(-coluna1,-coluna2)
#Seleciona as colunas que possuem uma palavra especifica
select(dataset, startsWith("Palavra")) #Instalação de pacote
install.packages("dplyr")
require("dplyr") #Chamando pacote

################################################
dados <- iris
names(dados) #Verifica o nome das colunas
nrow(dados) #Conta as linhas
row.names(dados) #Conta linhas e colunas
# filtra linhas
filter(dados, Species=="setosa", Petal.Length>1.5)
# ordena registros
arrange(dados, Species, desc(Sepal.Length))
# seleciona colunas
select(dados, Species, Petal.Width)
select(dados, -Species, -Petal.Width)
select(dados, starts_with("Sepal"))
# cria colunas
mutate(dados,
       mult1 = Sepal.Width*Petal.Width,
       mult2 = Sepal.Width*Sepal.Length,
       mult3 = mult1*mult2)

#Com base nos dados mtcars, faça:
#  Armazene os dados em um novo objeto
#Verifique o nome das colunas com a função names()
#Quantas linhas existem na tabela?
#  Calcule a quantidade de registros com hp > 100
#Calcule a quantidade de registros com hp > 100 e wt > 2700
#Ordene os registros por mpg e wt descendente
#Selecione apenas as colunas mpg e cyl
#Crie duas novas colunas: mpg^2 e hp/wt

dados <- mtcars
names(dados) #Verifica o nome das colunas
nrow(dados) #Conta as linhas
row.names(dados) #Conta linhas e colunas

#transformando variasveis
as.character(df$nome) #Transformando para caracter
as.numeric(df$nome) #Transformando para tipo númerica

#Verificando se existe N/A
is.na(df) 
sum(is.na(df)) #Somando quantidades de NA
table(is.na(df))

#Calculando a média da idade
mean(df$idade)
mean(df$idade, na.rm=T) #na.rm remove campos nulos

#Aplicando NA para o primeito registro da variavel
df$idade[1] <- NA

#comando for syntaxe 
x <- 1
for (i in 1:10){
  print(x[i])
}

install.packages("titanic")



treino <- titanic::titanic_train



str(treino)


install.packages("titanic")

#############################RESPOSTAS


# Qual a quantidade de registros da tabela?
treino <- titanic::titanic_train
str(treino)
nrow(treino) 

# Qual a quantidade de colunas da tabela?
row.names(treino)

# Qual a distribuição da variável Sex?
tablle(treino$Sex)


# Analise a distribuição da variável Age
summary(treino$Age)
hist(treino$Age)

# Qual a idade média dos homens? E das mulheres?
mean(treino$Age[treino$Sex=="male"], na.rm=TRUE)
mean(treino$Age[treino$Sex=="female"], na.rm = TRUE)


# Analise a distribuição da variável Fare
names(treino)
hist(treino$Fare)
plot(treino$Fare, type="s")

# Remova os registros com Embarked = nulo
treino <- treino[treino$Embarked!="",]

# Qual a distrubição de frequencias de Survived?
table(treino$Survived)

# Qual a distribuição de frequencias de Survived por sexo? O que podemos
# dizer sobre a probabilidade de sobreviver em cada um dos sexos ?
(tabela <- table(treino$Sex, treino$Survived))
(ptabela <- prop.table(tabela, margin = 1))
plot(ptabela)

# Crie uma variável que identifica se o indivíduo é adulto (tem pelo menos 18 anos)
# ou criança (tem menos de 18 anos).
treino$adulto[treino$Age >= 18] <- "Adulto"
treino$adulto[treino$Age < 18] <- "Criança"
treino$adulto
plot(prop.table(table(treino$adulto, 
                      treino$Survived, margin = 1))

#Analise a distribuição de frequência dessa variável. O que podemos dizer sobre
# a probabilidade de sobreviver entre crianças e adultos?



# Como você avalia a taxa de sobrevivência em função do preço pago

# na passagem (fare)?



# Como você avaliaria a relação entre Pclass e a taxa de sobrevivência? Apoie

# sua análise a partir de um gráfic

