CREATE TABLE popular_movies.movies(
	movie_id SERIAL PRIMARY KEY,
	title TEXT NOT NULL,
	release_year INT NOT NULL
);

CREATE TABLE popular_movies.people(
	person_id SERIAL PRIMARY KEY,
	name TEXT NOT NULL UNIQUE
);

CREATE TABLE popular_movies.ratings(
	rating_id SERIAL PRIMARY KEY,
	person_id INT NOT NULL,
	movie_id INT NOT NULL,
	-- Assume that everyone answering the survey has watched every movie
	rating INT NOT NULL CHECK (rating BETWEEN 1 AND 5),
	rated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,

	CONSTRAINT unique_person_movie UNIQUE (person_id, movie_id),
	
	CONSTRAINT fk_person
		FOREIGN KEY (person_id)
		REFERENCES popular_movies.people(person_id),
		
	CONSTRAINT fk_movie
		FOREIGN KEY (movie_id)
		REFERENCES popular_movies.movies(movie_id)
);