Przykład 1

Dane pochodzą z badania blisko 54000 diamentów w roku 2008 [@wickhamGgplot2CreateElegant2018]. Objęło 10 parametrów: cenę (price), wagę (carat), jakość cięcia (cut), kolor (color), przejrzystość (clarity) oraz kilka parametrów określających wymiary diamentu przedstawionych na @fig-1

{r}
#| warning: false
#| label: fig-1
#| fig-cap: "Wymiary diamentu"
library(gtsummary)
library(rstatix)
library(knitr)
library(easystats)
library(jtools)

include_graphics("diamenty1.png")

Załóżmy, że naszym zadaniem jest oszacowanie wpływu poszczególnych cech diamentu na jego cenę.

Dobierzmy trzy różne próby:
  
  losową 1 - zawierającą 1000 wylosowanych obserwacji z całego zbioru

losową 2 - zawierającą 1000 wylosowanych obserwacji z całego zbioru

nielosową - zawierającą 1000 pierwszych obserwacji z całego zbioru

Dobieramy dwie próby losowe i jedna nielosową, aby pokazać różnice pomiędzy nimi. Spodziewamy się, że próby losowe nie będą się różniły od siebie istotnie statystycznie, natomiast próba nielosowa powinna różnić się od obu prób losowych w sposób istotny statystycznie.

{r}
set.seed(44)
proba_losowa1 <- sample_n(diamonds, size = 1000)
proba_losowa2 <- sample_n(diamonds, size = 1000)
proba_nielosowa <- slice_head(diamonds, n = 1000)

razem <- bind_rows(proba_losowa1, proba_losowa2, proba_nielosowa) |> 
  mutate(proba = rep(c("losowa1", "losowa2", "nielosowa"), each = 1000))

{r}
#| label: fig-2
#| fig-height: 7
#| fig-cap: "Porównanie prób w kontekście wagi"
razem |> 
  ggbetweenstats(x = proba,
                 y = carat,
                 bf.message = F)

{r}
#| label: fig-3
#| fig-height: 8
#| fig-cap: "Porównanie prób w kontekście jakości cięcia"

razem |> 
  ggbarstats(x = cut,
             y = proba,
             bf.message = F,proportion.test = F)

{r}
#| label: fig-4
#| fig-height: 7
#| fig-cap: "Porównanie prób w kontekście ceny"
razem |> 
  ggbetweenstats(x = proba,
                 y = price,
                 bf.message = F)

Wnioski

zauważalne istotne różnice pomiędzy zmiennymi niezależnymi (parametry diamentów) w porównaniu prób losowych z nielosową;

brak istotnych różnic pomiędzy zmiennymi niezależnymi w porównaniu prób losowych;

zauważalne istotne różnice poziomów zmiennej zależnej (cena) w porównaniu prób losowych z nielosową;

brak istotnych różnic pomiędzy poziomów zmiennej zależnej w porównaniu prób losowych;

{r}
#| label: tbl-1
#| tbl-cap: Porównanie efektów
#| results: asis
mod1 <- lm(price~carat, data = proba_losowa1)
mod2 <- lm(price~carat, data = proba_losowa2)
mod3 <- lm(price~carat, data = proba_nielosowa)
export_summs(mod1,mod2,mod3, model.names = c("losowa1", "losowa2", "nielosowa"))

{r}
#| label: fig-5
#| fig-cap: Porównanie efektów
#| fig-height: 7

plot_summs(mod1,mod2,mod3, model.names = c("losowa1", "losowa2", "nielosowa"))

{r}
#| label: fig-6
#| fig-cap: Porównanie efektów addytywnych z brzegowymi
#| fig-height: 7
#| 
mod1_add <- lm(price~carat+x+y+z, data = proba_losowa1)
mod2_add <- lm(price~carat+x+y+z, data = proba_losowa2)
mod3_add <- lm(price~carat+x+y+z, data = proba_nielosowa)
plot_coefs(mod1, mod3, mod1_add, mod3_add,
           model.names = c("losowa1", "nielosowa", "losowa1 addytywny", "nielosowa addytywny"), coefs = c("carat"))

Źródła