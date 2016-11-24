--Solution #1: From Aaron Bertrand’s Article

SELECT ID, Median = AVG(1.0 * N)
FROM
(
   SELECT ID, o.N, rn=ROW_NUMBER() OVER (PARTITION BY ID ORDER BY o.N), c.c
   FROM #MedianValues AS o
   CROSS APPLY (SELECT c=COUNT(*) FROM #MedianValues b WHERE o.ID=b.ID) AS c
) AS x
WHERE rn IN ((c + 1)/2, (c + 2)/2)
GROUP BY ID;


-- solution #2
SELECT Median = AVG(1.0 * Val)
FROM 
(
  SELECT val, 
     c  = COUNT(*) OVER (),
     rn = ROW_NUMBER() OVER (ORDER BY val)
  FROM dbo.EvenRows
) AS x
WHERE rn IN ((c + 1)/2, (c + 2)/2);
