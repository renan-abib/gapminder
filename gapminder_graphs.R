library(dslabs)
data(gapminder)
library(dplyr)
library(ggplot2)


# Para remover elementos ou linhas duplicadas
unique(gapminder$country)


# Utilizando a coluna gdp, realize o c�lculo gdp/population, para obter, em
# m�dia, o valor que uma pessoa recebe por ano no pa�s em quest�o.
# Adicione esta nova coluna (personIncome) no seu dataset

gapminder2 <- gapminder %>% mutate(personIncome = gdp/population)

# Que tipo de informa��o pode-se obter de personIncome considerando-se o
# boxplot desses valores por continente, nos anos de 1962 e 2010?


# Maneira alternativa de se criar filtros
# gapminder_1962<- filter(gapminder2, year%in%c(1962))
# gapminder_2010<- filter(gapminder2, year%in%c(2010))

# Filtrando o ano de 1962
gapminder_1962<- filter(gapminder2, year == 1962)

# Filtrando o ano de 2010
gapminder_2010<- filter(gapminder2, year == 2010)

# Boxplot do ano de 1962
boxplot(gapminder_1962$personIncome~gapminder_1962$continent, main = 1962, xlab = "Continente", ylab = "Person Income", ylim = range(0:40000))

# Boxplot do ano de 1962 (sem escala modificada)
boxplot(gapminder_1962$personIncome~gapminder_1962$continent, main = 1962, xlab = "Continente", ylab = "Person Income")

# Boxplot do ano de 2010
boxplot(gapminder_2010$personIncome~gapminder_2010$continent, main = 2010, xlab = "Continente", ylab = "Person Income", ylim = range(0:40000))

# Boxplot do ano de 2010 (sem escala modificada)
boxplot(gapminder_2010$personIncome~gapminder_2010$continent, main = 2010, xlab = "Continente", ylab = "Person Income")

# Compare (gr�fico de pontos) personIncome e expectativa de vida nos
# continentes em 1962 e 2010. O que se pode concluir dessa an�lise?

# Gr�fico de pontos entre personIncome e expectativa de vida para o ano de 1962
ggplot(gapminder_1962, aes(personIncome, life_expectancy, color = continent)) +
  geom_point() + ggtitle("1962") +
  xlab("personIncome") + ylab("Expectativa de vida") + xlim(0, 50000) + ylim(0,100)

# Gr�fico de pontos entre personIncome e expectativa de vida para o ano de 2010
ggplot(gapminder_2010, aes(personIncome, life_expectancy, color = continent)) +
  geom_point() + ggtitle("2010") +
  xlab("personIncome") + ylab("Expectativa de vida") + xlim(0, 50000) + ylim(0,100)

# Filtrando os anos de 1962 e 2010
data_1962_2010<- filter(gapminder2, year %in% c("1962", "2010"))

# Plotando o gr�fico personIncome e expectativa de vida com anos filtrados
ggplot(data_1962_2010, aes(personIncome, life_expectancy, color = continent)) +
  geom_point() +
  xlab("personIncome") + ylab("Expectativa de vida") + xlim(0, 50000) + ylim(0,100) +
  facet_grid(.~year) 

# Caso quisesse representar separadamente por continente, mudar o faceset grid
ggplot(data_1962_2010,aes(personIncome, life_expectancy, color = continent)) +
  geom_point() +
  xlab("personIncome") + ylab("Expectativa de vida") +
  facet_grid(continent~year)

# Compare personIncome e taxa de mortalidade infantil (gr�fico de pontos) nos
# continentes em 1962 e 2010. H� alguma diferen�a not�vel?

ggplot(data_1962_2010, aes(personIncome, infant_mortality, color = continent)) +
  geom_point() +
  xlab("personIncome") + ylab("Mortalidade infantil") + xlim(0, 50000) +
  facet_grid(.~year)
