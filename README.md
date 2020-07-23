# MovieLens Recommendation System
Every day, the demand for data and artifical intelligence is growing. Automation is becoming almost a necessity in today's markets. Predicting outcomes accuracy is also desired, however the results aren't always perfect. Despite this, companies collect data which is then used to implement such systems. In the context of streaming services such as Netflix and Hulu, user ratings for movies are used to build movie reccomendation systems.

In this report, the goal was to **implement a movie reccomendation system** using the MovieLens dataset, which contains about 10 million user ratings. The ratings are represented using a 5-star system, from 1 being the worst rating to 5 being the best. In the dataset, we are also given the movie IDs, the user IDs, the movie title (with the release year attached to them), the genre(s), and the time in which was rating was given. In our analysis, we were able to discover key trends in out dataset's features, such as the variability of ratings across users and genres.

Using these patterns, we implemented numberous models to predict the movie ratings. We experimented with the various features to see how much of an effect they had on the RMSE. To prevent overfitting, we split the dataset into a training dataset (edx) and a test set (validation). All records in the validation set are also in the edx dataset to ensure we can properly predict their ratings. The edx dataset consisted of approximately 90% of MovieLens dataset, or 9 million records. The validation set consisted of the of the other 10%, or nearly 1 million records. 

We also used regularization to help improve our prediction. Using this method, we were able to achieve a **residual mean squared error (RMSE) of 0.8644229** using a regularized model of the movie, user, release year, and genre effects. However, to implement this model, the analysis resulted in roughly 25GB of RAM. Therefore, devices that do not have sufficient memory will crash. Nevertheless, the results are shared in this document.

Each section has their methods and models explained, followed by their respective results. 

The dataset can be accessed here: <https://grouplens.org/datasets/movielens/10m/>
