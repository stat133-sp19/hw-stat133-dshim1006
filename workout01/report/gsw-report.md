workout01-Jennifer-Shim.Rmd
================
Jennifer Shim
3/6/2019

Golden State Warriors' Five Heros
=================================

**Introduction**

The Golden State Warriors, founded in 1946 in Philadelphia, proudly show their great existence in the National Basketball Association (NBA) today. They had their first championship in 1947, second championship in 1956, and the third one in 1975. However, their struggles to capture another championship resumed until 2010s, before the era of Stephen Curry came. With all-stars Klay Thompson and Draymond Green, the team returned to the championship glory in 2015 and more winnings in 2017 and 2018 with Kevin Durant and Andre Iguodala. Now, we are going to take a closer look at five rising stars -- Andre Iguodala, Draymond Green, Kevin Durant, Klay Thompson, and Stephen Curry -- about their basic backgrounds, achievements, and comparison between each players specifically in year 2016.

**Backgrounds About Each Player**

<img src="hw-stat133/workout01/images/stephencurry1.jpg" width="80%" style="display: block; margin: auto;" /> **Stephen Curry** Stephen Curry, born in March 14, 1988, is the Point Guard player for the Golden State Warriors since 2009. His height and weight are recorded as 6 ft 3 in and 190 lbs correspondingly. He has been called for the Most Valuable Player (MVP) twice and won three NBA championships as a Warrior. With his incredible three-point shot, he is recognized as a star who brought revolution in basketball game. In 2014-15, he has led the team to gain their first Championship after their third won in 1975.Curry is also a son of the former NBA player, Dell Curry and older brother of current NBA player Seth Curry.

<img src="hw-stat133/workout01/images/draymondgreen.jpg" width="80%" style="display: block; margin: auto;" />

**Draymond Green** Draymond Green, born in March 4, 1990, is the Power Forward player for the Golden State Warriors since 2012. His height and weight are recorded as 6ft 7in and 230 lbs correspondingly. He has won three-time NBA All-Star and NBA Defensive Player of the Year in 2017. His potential as a talented basketball player shines during his high school and college years. As a senior in college, he wins NABC National Player of the Year honor and plus joins the GSW upon the graduation.

<img src="hw-stat133/workout01/images/andreiguodala.jpg" width="80%" style="display: block; margin: auto;" /> **Andre Iguodala** Andre Iguodala, born in January 28, 1984, is the Shooting Guard/Small Forward player for the Golden State Warrior since 2013. His height and weight are recorded as 6ft 6in and 215 lbs correspondingly. Before he joins the Warrior, he plays in the Philadelphia 76ers and in 2015, he gains the title NBA Finals Most Valuable Player.

<img src="hw-stat133/workout01/images/kevindurant.jpg" width="80%" style="display: block; margin: auto;" /> **Kevin Durant** Kevin Durant, born in September 29, 1988, is the Small Forward player for the Golden State Warrior since 2016. His height and weight are 6ft 9in and 240 lbs correspondingly. He plays in Oklahoma CIty before signing with GSW. He has been called for two Finals MVP awards and two NBA All-Star Game Most Valuable Player Awards and so many more.

<img src="hw-stat133/workout01/images/klaythompson.jpg" width="80%" style="display: block; margin: auto;" />

**Klay Thompson** Klay Thompson, born in February 8, 1990, is the Shooting Guard player for the Golden State Warriors since 2011. His height and weights are recorded as 6ft 7in and 215 lbs correspondingly. Like Stephen Curry, Thompson is also the son of former NBA player Mychal Thompson. He takes a key role in winning championship in 2017-18.

``` r
library(ggplot2)
library(jpeg)
library(grid)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
dat <-read.csv("hw-stat133/workout01/data/shots-data.csv", stringsAsFactors = FALSE)

prep1 <- group_by(dat, player) %>% filter(shot_type == "2PT Field Goal")
two_pt <- summarise(prep1, total = length(shot_made_flag), made = sum(shot_made_flag == "shot_yes"), perc_made = (made / total))
two_pt <-arrange(two_pt, desc(perc_made))

prep2 <- group_by(dat, player) %>% filter(shot_type == "3PT Field Goal")
three_pt <- summarise(prep2, total = length(shot_made_flag), made = sum(shot_made_flag == "shot_yes"), perc_made = (made / total))
three_pt <- arrange(three_pt, desc(perc_made))

prep3 <- group_by(dat, player)
overall <- summarise(prep3, total = length(shot_made_flag), made = sum(shot_made_flag == "shot_yes"), perc_made = (made / total))
overall <- arrange(overall, desc(perc_made))
```

After Kevin Durant joins the Golden State Warrior, the team become most powerful than ever. Even though there are slight differences in shooting points between each individual players, we are going to display those small differences using data tables and graphs.

``` r
two_pt
```

    ## # A tibble: 5 x 4
    ##   player         total  made perc_made
    ##   <chr>          <int> <int>     <dbl>
    ## 1 Andre Iguodala   210   134     0.638
    ## 2 Kevin Durant     643   390     0.607
    ## 3 Stephen Curry    563   304     0.540
    ## 4 Klay Thompson    640   329     0.514
    ## 5 Draymond Green   346   171     0.494

Above data table displays two points effective shooting percent by each player. Andre Iguodala records the most shots made out of the total attempts made and Draymond Green records the least effective shooting scores.The difference between the highest and the lowest percent is almost 14 percents.

``` r
three_pt
```

    ## # A tibble: 5 x 4
    ##   player         total  made perc_made
    ##   <chr>          <int> <int>     <dbl>
    ## 1 Klay Thompson    580   246     0.424
    ## 2 Stephen Curry    687   280     0.408
    ## 3 Kevin Durant     272   105     0.386
    ## 4 Andre Iguodala   161    58     0.360
    ## 5 Draymond Green   232    74     0.319

Above data table displays three points effective shooting percent by each player. Klay Thompson records the most shots made out of the total attempts made and Draymond Green, again, records the least effective scores. The difference between the highest and the lowest percent is 11 percent, which is little bit higher than the two points shooting.

``` r
overall
```

    ## # A tibble: 5 x 4
    ##   player         total  made perc_made
    ##   <chr>          <int> <int>     <dbl>
    ## 1 Kevin Durant     915   495     0.541
    ## 2 Andre Iguodala   371   192     0.518
    ## 3 Klay Thompson   1220   575     0.471
    ## 4 Stephen Curry   1250   584     0.467
    ## 5 Draymond Green   578   245     0.424

Above data table displays the overall effective shooting percent by each player in 2016. Kevin Durant records the most shots made out of the total attempts made and Draymond Green, records the least effective scores. The differences between the highest and the lowest percent is 12.

Based on these three data tables, we can conclude that depending on the positions the player play for, the number successful shooting can differ noticeably. Since Kevin Durant, who takes the position as Small Forward (mainly focuses on shooting), and therefore, makes the most effective shots. Draymond Green, on the other hand, takes the position in Power Forward, who position oneself under the basket and defense against the opponents.

<embed src="hw-stat133/workout01/images/gsw-shot-charts.pdf" width="80%" style="display: block; margin: auto;" type="application/pdf" />
Above graph displays the locations where the shots made and wether the shot is successfuly made or failed in that locations.

**Analysis**

**Andre Iguodala**

Overall, Andre Iguodala has the least shooting number out of all five players despite the fact that he is a Shooting Guard/Small Forward. His graph indicates that he is better in shooting from the three points position rather than from the two points shooting position.

**Draymond Green**

According to the Green’s graph, we can assume that Green is not an effective shooter near the court net and also around the three pointer. Considering that his position is Power Forward, his shooting skill is relatively low compared to other players who take positions as Small Forward and Shooting Guard.

**Kevin Durant**

Durant has the second most shooting attempts. Durant can be seen as almost exactly opposite of Stephen Curry. While Curry makes an effective shots around three pointer, Durant makes his effective shots near two pointer.

**Klay Thompson**

Klay Thompson seems to be the most effective shooter out of all five players because more green dots are spotted than the red dots. These dots are evenly spreaded out but slightly concentrated more in the corners than the center. He also makes the most shot attempts.

**Stephen Curry**

Stephen Curry makes the most successful and the most number of shots as well as Kevin Durant. But unlike Thompson, his successful shots are concentrated around the three pointer. As a Point Guard, who mainly focuses on assisting the Shooting Guard, his skill in shooting is truly talented.

**Conclusion**

All in all, this article serves to provide detailed information on five basketball stars from Golden State Warriors including their backgrounds and performance during the game in year 2016. Based on all the data tables and graphs provided, the audience can make validation whether the position of the player suits well or not. Especially for Small Forward and Shooting Guard, if their performances on shooting outstands the other player, then they have the right position. Also, in order to make a conclusion about who’s the best player in the year 2016, we have to take a look at all the data gathered. In the data tables that show overall effective shooting percent and two points effective shooting percent by each player, Andre Iguodala places first and second. However, this is because his total attempted throw is significantly lower than other players. Since making conclusion takes time and careful attention, we cannot generalize the result just by looking the one table. The scores do not really measure their performance because they change every year. Regardless of their scores, the audience's expectation towards these five players will continue to rise.
