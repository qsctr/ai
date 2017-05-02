# Language guess

See the source code on GitHub: https://github.com/qsctr/language-guess

## 1/6/2017

This semester (or at least this quarter) we are required to use Python for our projects. This project is quite simple and is meant to get us familiar with Python.

The goal is to analyze some text and determine the language it is written in. Human language, that is, not programming language. This is done by finding the frequency of each letter in the text and comparing them to the average frequencies of letters of many languages and finding the closest language. So this method only works for European languages which use alphabets, and not other languages like Chinese.

I am storing the data in a CSV, or Comma Separated Values, file. Which, if you don't know what CSV is, is exactly what you would expect.

```
Letter,English,French,German,Spanish,Portuguese,Esperanto,Italian,Turkish,Swedish,Polish,Dutch,Danish,Icelandic,Finnish,Czech
a,8.167,7.636,6.516,11.525,14.634,12.117,11.745,12.920,9.383,10.503,7.486,6.025,10.110,12.217,8.421
b,1.492,0.901,1.886,2.215,1.043,0.980,0.927,2.844,1.535,1.740,1.584,2.000,1.043,0.281,0.822
c,2.782,3.260,2.732,4.019,3.882,0.776,4.501,1.463,1.486,3.895,1.242,0.565,0,0.281,0.740
d,4.253,3.669,5.076,5.010,4.992,3.044,3.736,5.206,4.702,3.725,5.933,5.858,1.575,1.043,3.475
e,12.702,14.715,16.396,12.181,12.570,8.995,11.792,9.912,10.149,7.352,18.91,15.453,6.418,7.968,7.562
f,2.228,1.066,1.656,0.692,1.023,1.037,1.153,0.461,2.027,0.143,0.805,2.406,3.013,0.194,0.084
g,2.015,0.866,3.009,1.768,1.303,1.171,1.644,1.253,2.862,1.731,3.403,4.077,4.241,0.392,0.092
h,6.094,0.737,4.577,0.703,0.781,0.384,0.636,1.212,2.090,1.015,2.380,1.621,1.871,1.851,1.356
i,6.966,7.529,6.550,6.247,6.186,10.012,10.143,9.600,5.817,8.328,6.499,6.000,7.578,10.817,6.073
j,0.153,0.613,0.268,0.493,0.397,3.501,0.011,0.034,0.614,1.836,1.46,0.730,1.144,2.042,1.433
k,0.772,0.049,1.417,0.011,0.015,4.163,0.009,5.683,3.140,2.753,2.248,3.395,3.314,4.973,2.894
l,4.025,5.456,3.437,4.967,2.779,6.104,6.510,5.922,5.275,2.564,3.568,5.229,4.532,5.761,3.802
m,2.406,2.968,2.534,3.157,4.738,2.994,2.512,3.752,3.471,2.515,2.213,3.237,4.041,3.202,2.446
n,6.749,7.095,9.776,6.712,4.446,7.955,6.883,7.987,8.542,6.237,10.032,7.240,7.711,8.826,6.468
o,7.507,5.796,2.594,8.683,9.735,8.779,9.832,2.976,4.482,6.667,6.063,4.636,2.166,5.614,6.695
p,1.929,2.521,0.670,2.510,2.523,2.755,3.056,0.886,1.839,2.445,1.57,1.756,0.789,1.842,1.906
q,0.095,1.362,0.018,0.877,1.204,0,0.505,0,0.020,0,0.009,0.007,0,0.013,0.001
r,5.987,6.693,7.003,6.871,6.530,5.914,6.367,7.722,8.431,5.243,6.411,8.956,8.581,2.872,4.799
s,6.327,7.948,7.270,7.977,6.805,6.092,4.981,3.014,6.590,5.224,3.73,5.805,5.630,7.862,5.212
t,9.056,7.244,6.154,4.632,4.336,5.276,5.623,3.314,7.691,2.475,6.79,6.862,4.953,8.750,5.727
u,2.758,6.311,4.166,2.927,3.639,3.183,3.011,3.235,1.919,2.062,1.99,1.979,4.562,5.008,2.160
v,0.978,1.838,0.846,1.138,1.575,1.904,2.097,0.959,2.415,0.012,2.85,2.332,2.437,2.250,5.344
w,2.360,0.074,1.921,0.017,0.037,0,0.033,0,0.142,5.813,1.52,0.069,0,0.094,0.016
x,0.150,0.427,0.034,0.215,0.253,0,0.003,0,0.159,0.004,0.036,0.028,0.046,0.031,0.027
y,1.974,0.128,0.039,1.008,0.006,0,0.020,3.336,0.708,3.206,0.035,0.698,0.900,1.745,1.043
z,0.074,0.326,1.134,0.467,0.470,0.494,1.181,1.500,0.070,4.852,1.39,0.034,0,0.051,1.503
à,0,0.486,0,0,0.072,0,0.635,0,0,0,0,0,0,0,0
â,0,0.051,0,0,0.562,0,0,0,0,0,0,0,0,0,0
á,0,0,0,0.502,0.118,0,0,0,0,0,0,0,1.799,0,0.867
å,0,0,0,0,0,0,0,0,1.338,0,0,1.190,0,0.003,0
ä,0,0,0.578,0,0,0,0,0,1.797,0,0,0,0,3.577,0
ã,0,0,0,0,0.733,0,0,0,0,0,0,0,0,0,0
ą,0,0,0,0,0,0,0,0,0,0.699,0,0,0,0,0
æ,0,0,0,0,0,0,0,0,0,0,0,0.872,0.867,0,0
œ,0,0.018,0,0,0,0,0,0,0,0,0,0,0,0,0
ç,0,0.085,0,0,0.530,0,0,1.156,0,0,0,0,0,0,0
ĉ,0,0,0,0,0,0.657,0,0,0,0,0,0,0,0,0
ć,0,0,0,0,0,0,0,0,0,0.743,0,0,0,0,0
č,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.462
ď,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.015
ð,0,0,0,0,0,0,0,0,0,0,0,0,4.393,0,0
è,0,0.271,0,0,0,0,0.263,0,0,0,0,0,0,0,0
é,0,1.504,0,0.433,0.337,0,0,0,0,0,0,0,0.647,0,0.633
ê,0,0.218,0,0,0.450,0,0,0,0,0,0,0,0,0,0
ë,0,0.008,0,0,0,0,0,0,0,0,0,0,0,0,0
ę,0,0,0,0,0,0,0,0,0,1.035,0,0,0,0,0
ě,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1.222
ĝ,0,0,0,0,0,0.691,0,0,0,0,0,0,0,0,0
ğ,0,0,0,0,0,0,0,1.125,0,0,0,0,0,0,0
ĥ,0,0,0,0,0,0.022,0,0,0,0,0,0,0,0,0
î,0,0.045,0,0,0,0,0,0,0,0,0,0,0,0,0
ì,0,0,0,0,0,0,0.030,0,0,0,0,0,0,0,0
í,0,0,0,0.725,0.132,0,0,0,0,0,0,0,1.570,0,1.643
ï,0,0.005,0,0,0,0,0,0,0,0,0,0,0,0,0
ı,0,0,0,0,0,0,0,5.114,0,0,0,0,0,0,0
ĵ,0,0,0,0,0,0.055,0,0,0,0,0,0,0,0,0
ł,0,0,0,0,0,0,0,0,0,2.109,0,0,0,0,0
ñ,0,0,0,0.311,0,0,0,0,0,0,0,0,0,0,0
ń,0,0,0,0,0,0,0,0,0,0.362,0,0,0,0,0
ň,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.007
ò,0,0,0,0,0,0,0.002,0,0,0,0,0,0,0,0
ö,0,0,0.443,0,0,0,0,0.777,1.305,0,0,0,0.777,0.444,0
ô,0,0.023,0,0,0.635,0,0,0,0,0,0,0,0,0,0
ó,0,0,0,0.827,0.296,0,0,0,0,1.141,0,0,0.994,0,0.024
ø,0,0,0,0,0,0,0,0,0,0,0,0.939,0,0,0
ř,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.380
ŝ,0,0,0,0,0,0.385,0,0,0,0,0,0,0,0,0
ş,0,0,0,0,0,0,0,1.780,0,0,0,0,0,0,0
ś,0,0,0,0,0,0,0,0,0,0.814,0,0,0,0,0
š,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.688
ß,0,0,0.307,0,0,0,0,0,0,0,0,0,0,0,0
ť,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.006
þ,0,0,0,0,0,0,0,0,0,0,0,0,1.455,0,0
ù,0,0.058,0,0,0,0,0.166,0,0,0,0,0,0,0,0
ú,0,0,0,0.168,0.207,0,0,0,0,0,0,0,0.613,0,0.045
û,0,0.060,0,0,0,0,0,0,0,0,0,0,0,0,0
ŭ,0,0,0,0,0,0.520,0,0,0,0,0,0,0,0,0
ü,0,0,0.995,0.012,0.026,0,0,1.854,0,0,0,0,0,0,0
ů,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.204
ý,0,0,0,0,0,0,0,0,0,0,0,0,0.228,0,0.995
ź,0,0,0,0,0,0,0,0,0,0.078,0,0,0,0,0
ż,0,0,0,0,0,0,0,0,0,0.706,0,0,0,0,0
ž,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.721
```

The data itself is from the [Wikipedia article on letter frequency](https://en.wikipedia.org/wiki/Letter_frequency).

Here is a Python (3) function that returns the name of the language, given a filename to read the text from.

```python
def guess_language(filename):
    with open(filename, encoding='utf-8') as textfile, open('data.csv', encoding='utf-8') as data:
        (_, *letters), *rows = zip(*[line.split(',') for line in data.read().splitlines()])
        text = ''.join(filter(letters.__contains__, textfile.read().casefold()))
    textfreqs = [text.count(let) / len(text) * 100 for let in letters]
    diffs = {lang: sum(map(lambda x, y: abs(float(x) - y), freqs, textfreqs)) for lang, *freqs in rows}
    return min(diffs, key=diffs.get)
```

Here is an explanation of the code.

Line 1:

Declare a function called `guess_language` with one parameter `filename`.

Line 2:

Open the filename given as the argument.

Open the data table.

Line 3:

Read the data table into a string.

Split the table on newlines, so it is now a list of strings.

For each string in the table, split the string on commas, so it is now a list of lists of strings (i.e. a matrix).

Pass each small list in the big list as an argument to the `zip` function, which returns a list with for each index i a tuple with for each list argument the element at index i, in effect transposing the list of lists into a list of tuples.

Assign the first element of the first tuple of the list to `_`, because it is not needed (it is the string `'Letter'`).

Collect the rest of the first tuple into a list and assign it to `letters`. `letters` is now a list of the letters in the table (`['a', 'b', ... 'ž']`).

Assign the rest of the list to `rows`. `rows` is now a list of tuples of the columns in the original table, except the first column (`[['English', '8.167', '1.492', ... '0'], ... ['Czech', '8.421', 0.822', ... '0.721']]`). It is called `rows` because the table was transposed.

Line 4:

Read the contents of the file given into a string.

Casefold the string, which is like lowercasing.

Filter the string so only characters which are in `letters` remain.

Assign the string to `text`.

Line 5:

Create a list with for each letter in `letters` the frequency of that letter in the text, which is the count of the letter in `text` divided by the total length of `text`, multiplied by 100 to get the percentage.

Assign the list to `textfreqs`.

Line 6:

For each tuple in `rows`, call the first element of the tuple `lang` and collect the rest of the tuple into a list called `freqs`. `lang` is the name of the language, and `freqs` is the frequencies of each letter for that language.

For each frequency in `freqs`, parse it into a float, and find the absolute difference of it and the corresponding frequency of that letter from the text.

Sum up all the differences of the frequencies for all letters so the result is the total difference between that language and the text.

Put it into a dictionary with `lang` as the key and the sum as the value.

Assign the complete dictionary with all the languages and their summed differences to `diffs`.

Line 7:

Find the key in `diffs` with the smallest corresponding value.

Return the key.

## 1/10/2017

Similarity measures are functions that measure how similar two things are. There are many different types of similarity measures.

```python
from math import sqrt
from operator import mul

def manhattan_distance(xs, ys):
    return sum(map(lambda x, y: abs(x - y), xs, ys))

def euclidean_distance(xs, ys):
    return sqrt(sum(map(lambda x, y: (x - y) ** 2, xs, ys)))

def chebyshev_distance(xs, ys):
    return max(map(lambda x, y: abs(x - y), xs, ys))

def cosine_similarity(xs, ys):
    return 1 - dot_product(xs, ys) / (sqrt(dot_product(xs, xs)) * sqrt(dot_product(ys, ys)))
    def dot_product(xs1, ys1):
        return sum(map(mul, xs1, ys1))
```

I tried different similarity measures for this project. Similarity measures are used when comparing the frequencies of letters in the text to analyze with the known average frequencies of a certain language, to see how close the text is to that language. Previously, in the code with the explanation above, I was using the manhattan distance.

I only did some limited testing manually, so it is hard to determine which one(s) are more accurate. From my testing it seemed like manhattan distance and euclidean distance were more accurate than the others, but I cannot be sure of it because I have not done extensive testing. If I had more time I would have written some automated tests to test the similarity measures with more data and get a more reliable result.