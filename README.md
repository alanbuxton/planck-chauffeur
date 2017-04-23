# Planck Knowledge and Chauffeur Knowledge

This project was inspired by the fabulous [Farnam Street's](https://www.farnamstreetblog.com/about/) post about the [Two Different Kinds of Knowledge]( www.farnamstreetblog.com/2015/09/two-types-of-knowledge/): On the one hand you have people who really know about a subject. The post refers to this type of knowledge as "Planck Knowledge". On the other hand are those people who know how to sound like they know the subject. These ones have "Chauffeur Knowledge".

Telling the difference between Chauffeur Knowledge and Planck Knowledge is a problem that plagues politics, business and media. How do you tell the difference between someone who has put the hard work in to truly understand a topic and someone who talks confidently based on an evening's internet research?

I've seen this problem a lot in my day job. Kathy Sierra summarised it perfectly in [Creating Passionate Users](http://headrush.typepad.com/). In particular:

![Which One Wins](http://headrush.typepad.com/photos/uncategorized/2007/04/06/glibwin.jpg)

So I built some software to help visualise the problem.

## How It Works

There are 6 parameters you can choose from:
* Planck Knowledge Mean
* Planck Knowledge Standard Deviation
* Chauffeur Knowledge Mean
* Chauffeur Knowlede Standard Deviation
* Number of people to ask
* Percentage chance of mistaking someone's Chauffeur Knowledge for their Planck Knowledge

Play around with the mean and standard deviations to get different results. The default uses a mean of 100 and standard deviation of 15, based on [how IQ works](https://en.wikipedia.org/wiki/Intelligence_quotient#Current_tests). I'm not sure whether IQ is better at measuring Planck or Chauffeur Knowledge but it seems a good enough starting point. It's a fun exercise to increase the Chauffeur Knowledge Mean on the basis that it's easier to fake knowledge on something than to really know something.

The model builds a network of however many people you want to ask. The default is 200 as this is somewhat over the [Dunbar number](https://en.wikipedia.org/wiki/Dunbar%27s_number)

Finally there is asking the question. As the asker, you may know someone well enough to know their true Planck Knowledge on a subject, Or you might mistakenly believe their
Chauffeur Knowledge is what they really know on the subject. If you happen to ask someone a question on a topic where they have a high Chauffer Knowledge but a low Planck Knowledge then you will get a bad answer. I'm sure we can all cite plenty of examples where that has happened.

So the last parameter is the percentage chance that we get it wrong and mistake somoene's Chauffeur Knowledge from their real Planck Knowledge. The default is 50%. If you change this to 100% then you will always believe the Chauffeur Knowledge (not good). If you change it to 0% then you will always get the best possible answer from the network.

## Technical Details

The app is built with:
* [Haskell](https://www.haskell.org/)
* [Scotty](https://github.com/scotty-web/scotty)
* [Blaze](https://hackage.haskell.org/package/blaze-html)
* [HighCharts](https://www.highcharts.com/)

To run the application:
```
stack exec planck-chauffeur-exe portNumber maxSizeOfNetwork
```

## Conclusions From The Model

The best case scenario is that you have a 0% chance of mistaking Chauffeur Knowledge for Planck Knowledge. In this case, as the network grows you are guaranteed to get a better and better quality answer.

All other scenarios are unpredictable. As you ask more people the quality of the answer you get jumps around _a lot_. The best available answer always gets better, and what you think you know gets better, but what you *really* know jumps around a lot depending on how many times you mistook someone's Chauffeur Knowledge for their Planck Knowledge.

Plenty of parallels there to the internet, Social Media, etc.

So if there is a conclusion to draw it is this: **Vet your sources.** If you let anyone into your network where you get their Chauffeur and Planck Knowledge mixed up, then you run the risk of going astray.

## Postscript

I highly recommend that you sign up to Farnam Street's newsletter [Brain Food](https://www.farnamstreetblog.com/newsletter/).

The word Chauffeur literally means ["heater-upper"](http://etymonline.com/index.php?term=chauffeur). It harks back to the days when cars were steam powered and needed heating up before they could be driven.
