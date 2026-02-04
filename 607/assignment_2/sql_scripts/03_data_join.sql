CREATE OR REPLACE VIEW popular_movies.v_ratings AS
SELECT
  p.name,
  m.title,
  m.release_year,
  r.rating,
  r.rated_at
FROM popular_movies.ratings r
JOIN popular_movies.people p USING (person_id)
JOIN popular_movies.movies m USING (movie_id);
