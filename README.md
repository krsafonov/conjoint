You can access the project [here](https://krsafonov.shinyapps.io/survey_app/) or [here (Milana's version)](https://milarsenteva.shinyapps.io/survey_app/).

The project contains two apps: conjoint app (survey_app) and post-experiment survey.

Ссылка на колаб, где анализируем данные: https://colab.research.google.com/drive/11dOXMCkApPW6Cn7Qo3-waqkPYv2nU4YP?usp=sharing

To deploy them:

1. Log in into your account in shinyapps

```
rsconnect::setAccountInfo(name='krsafonov',
                          token='...',
                          secret='...')
```

2. Run

```
deployApp("survey_app")
deployApp("post_survey_app")
```

Also, the apps require google authorization, the tokens must be saved in folder ```.secrets```.
