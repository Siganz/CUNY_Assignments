--00 Schema Creation
DROP SCHEMA IF EXISTS popular_movies CASCADE;
CREATE SCHEMA popular_movies;

--01 Table Creation
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
	rating INT CHECK (rating BETWEEN 1 AND 5),
	rated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,

	CONSTRAINT unique_person_movie UNIQUE (person_id, movie_id),
	
	CONSTRAINT fk_person
		FOREIGN KEY (person_id)
		REFERENCES popular_movies.people(person_id),
		
	CONSTRAINT fk_movie
		FOREIGN KEY (movie_id)
		REFERENCES popular_movies.movies(movie_id)
);

--02 Data Creation
TRUNCATE popular_movies.ratings, popular_movies.people, popular_movies.movies
RESTART IDENTITY;

INSERT INTO popular_movies.movies (title, release_year) VALUES
('One Battle After Another', 2025),
('Begonia', 2025),
('Wicked for Good', 2025),
('The Materialist', 2025),
('Sinners', 2025);

INSERT INTO popular_movies.people (name) VALUES
('Alex'),
('Bri'),
('Chen'),
('Devi'),
('Eli'),
('Fran');

INSERT INTO popular_movies.ratings (person_id, movie_id, rating) VALUES

-- Alex (1) did not watch movie 5
(1,1,5),
(1,2,4),
(1,3,4),
(1,4,3),
(1,5,NULL),

-- Bri (2) did not watch movie 4
(2,1,4),
(2,2,3),
(2,3,5),
(2,4,NULL),
(2,5,4),

-- Chen (3) did not watch movie 1
(3,1,NULL),
(3,2,5),
(3,3,4),
(3,4,3),
(3,5,4),

-- Devi (4) did not watch movie 2
(4,1,3),
(4,2,NULL),
(4,3,3),
(4,4,4),
(4,5,5),

-- Eli (5) did not watch movie 3
(5,1,4),
(5,2,3),
(5,3,NULL),
(5,4,5),
(5,5,3),

-- Fran (6) did not watch movie 5
(6,1,5),
(6,2,3),
(6,3,4),
(6,4,4),
(6,5,NULL);

--03 View Creation
-- raw retains nulls
CREATE OR REPLACE VIEW popular_movies.v_ratings_raw AS
SELECT
  p.name,
  m.title,
  m.release_year,
  r.rating,
  r.rated_at
FROM popular_movies.ratings r
JOIN popular_movies.people p ON p.person_id = r.person_id
JOIN popular_movies.movies m ON m.movie_id = r.movie_id;

-- clean filters rows where ratings = NULL
CREATE OR REPLACE VIEW popular_movies.v_ratings_clean AS
SELECT *
FROM popular_movies.v_ratings_raw
WHERE rating IS NOT NULL;


