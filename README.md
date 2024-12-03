# sentiment-analyzer

### Overview

Sentiment Analyzer is a web application written in Haskell that classifies the sentiment of text input. The application uses a Naive Bayes algorithm, trained on labeled data, to analyze text sentiment and classify it on a scale from strongly negative (0) to strongly positive (4). This application is publicly accessible at https://sentiment-analyzer-nadya-6ca2373a8d05.herokuapp.com.

<br>

### Installation and Setup

1. **Clone the repository**
    ```
    git clone https://github.com/nadyahoesin/sentiment-analyzer
    cd sentiment-analyzer
    ```

2. **Install dependencies**
    ```
    stack setup
    stack install
    ```

3. **Run the application locally**
    ```
    stack run
    ```

4. **Build and run the Docker container**
    ```
    docker build -t sentiment-analyzer .
    docker run -p 8080:8080 sentiment-analyzer
    ```

5. **Deploy to Heroku**
    ```
    heroku login
    heroku create sentiment-analyzer
    git push heroku main
    ```

<br>

### API Endpoint

`GET /`

- **Description**: Analyzes the sentiment of a given text and returns the sentiment score on a scale from 0 (strongly negative) to 4 (strongly positive).

- **Request**: A GET request with the following query parameter:
    - `text` (required): The input text to analyze

- **Response**: A JSON object with the following structure:
    ``` 
    {
        "sentiment": <sentiment_score>
        "text": "<input_text>",
    }
    ```
    - `sentiment`: An integer between 0 and 4 representing the sentiment score:
        - 0: Strongly negative
        - 1: Slightly negative
        - 2: Neutral
        - 3: Slightly positive
        - 4: Strongly positive
    - `text`: The input text provided in the request

<br>

**Example Usage**

**Request**:

```
curl -X GET "https://sentiment-analyzer-nadya-6ca2373a8d05.herokuapp.com/?text=Life%20is%20incredible"
```

**Response**:

```
{
    "sentiment": 4,
    "text": "Life is incredible"
}
```

<br>

### Lesson Learned about Functional Programming
This project is fully written in Haskell, a purely functional programming language. Here are some characteristics of functional programming that are present in the project:

1. **Pure Functions**

    A pure function always produces the same output for the same input and has no side effects. Functions like `tokenize`, `countTokensFreq`, and `computeTokenGivenClassProbs` do not modify external state and depend solely on their input parameters.

2. **Higher-Order Functions**

    Functional programming encourages the use of higher-order functions, which are functions that take other functions as arguments or return functions as results. The use of `foldr`, a higher-order function, in the function `countTokensFreq` demonstrates this concept. 

3. **Declarative Programming**

    Instead of explicitly defining the steps to manipulate data, functional programming describes what the result should be. The function `preprocessTrainingData` uses composition and pipelines to describe how to process data, rather than using loops or keeping track of states.

4. **Modularity**
   
    Functional programming encourages breaking down a bigger function into smaller, reusable functions. Functions like `separateByClass`, `computeClassProbs`, and `evaluateAccuracy` are simple and each has one distinct responsibility, which makes them modular and reusable.

5. **Type Safety and Pattern Matching**
    
    Haskell’s strong, static type system ensures correctness at compile time. The use of types like `WordFreqsByClass` and `TextSentiment` makes data types clear and reduces runtime errors.

<br>

### Analysis of differences between Functional Programming (Haskell) and Imperative Programming (Python)

**High-Level Differences**
   
| Aspect              | Functional Programming                                                        | Imperative Programming                                                     |
|---------------------|-----------------------------------------------------------------------------|-------------------------------------------------------------------------|
| Data Transformation | Emphasizes declarative pipelines, function composition, and immutability.   | Focuses on explicit loops, mutable state, and step-by-step computation. |
| State Management    | Immutable state passed explicitly through functions.                        | Relies on mutable collections like lists and dicts.                     |
| Style               | Declarative: describes what to compute, focusing on transformation of data. | Procedural: specifies how to compute, focusing on operations over data. |
| Control Flow        | Uses recursion, higher-order functions, and lazy evaluation.                | Uses explicit loops (for) and conditional branches (if).                |erences


<br>

Here are some examples of how Functional Programming and Imperative Programming differs, based on Haskell code from this project and Python code to achieve the same result.



a) **Preprocessing Data**

**Haskell**: Preprocesses data using pipelines and immutable structures.

```
preprocessTrainingData :: V.Vector TextSentiment -> WordFreqsByClass
preprocessTrainingData = removeUninformativeTokens . tMap (countTokensFreq . concatMap tokenize) . separateByClass
```

The transformation is expressed as a chain of function compositions: separating by class, tokenizing, counting frequencies, and removing uninformative tokens.
Immutability ensures no side effects.


**Python**: Uses explicit loops with mutable state to preprocess data.

```
def preprocess_training_data(data: List[TextSentiment]) -> WordFreqsByClass:
    positive_texts, negative_texts = [], []

    for sentiment, text in data:
        if sentiment == 4:
            positive_texts.append(text)
        else:
            negative_texts.append(text)

    pos_freqs = Counter(tokenize(" ".join(positive_texts)))
    neg_freqs = Counter(tokenize(" ".join(negative_texts)))
```

This function explicitly appends items to lists using a for loop, then processes these mutable structures. This approach might be easier to understand for imperative programmers but is less concise and reusable.

<br>

b) **Tokenization**

**Haskell**: Composes transformations with a single expression.
```
tokenize :: String -> [String]
tokenize = words . map toLower . filter (\c -> isAlpha c || c == ' ')
```

**Python**: Uses a loop to achieve similar results.

```
def tokenize(text: str) -> List[str]:
    result = []
    for char in text:
        if char.isalpha() or char == ' ':
            result.append(char.lower())
    return ''.join(result).split()
```

The loop in this function iteratively processes each character, altering state (`result` list) as it iterates.

<br>

c) **Naive Bayes Classifier**

**Haskell**: Uses pure function composition, higher-order functions, and mathematical expressions.

```
naiveBayesClassifier :: String -> WordFreqsByClass -> Int
naiveBayesClassifier text freqs = round $ 4.5 * probPos / (probPos + probNeg)
    where
        (probPos, probNeg) = tMap exp $ tZipWith (+) (tMap log $ computeClassProbs freqs) $ 
                            foldl1 (tZipWith (+)) [tMap log $ computeTokenGivenClassProbs token freqs | token <- tokenize text]
```

Probability calculations involve composing functions like `tMap` and `foldl1`, avoiding mutable state. Token probabilities are accumulated using list comprehension.

**Python**: Explicitly iterates over tokens and updates mutable variables.

```
def naive_bayes_classifier(text: str, freqs: WordFreqsByClass) -> int:
    tokens = tokenize(text)

    pos_count = sum(freqs[0].values())
    neg_count = sum(freqs[1].values())

    pos_log_prob, neg_log_prob = log(pos_count / (pos_count + neg_count)), log(neg_count / (pos_count + neg_count))

    for token in tokens:
        token_pos_count, token_neg_count = freqs[0].get(token, 0), freqs[1].get(token, 0)

        pos_log_prob += log((token_pos_count + 1) / (pos_count + 2))
        neg_log_prob += log((token_neg_count + 1) / (neg_count + 2))

    return round(4.5 * exp(pos_log_prob) / (exp(pos_log_prob) + exp(neg_log_prob)))
```

Each token is processed sequentially in a for loop, accumulating probabilities in mutable variables `(pos_log_prob, neg_log_prob)`.

<br>

d) **Evaluating Accuracy**

**Haskell**: Uses higher-order functions to define logic.

```
evaluateAccuracy :: V.Vector TextSentiment -> WordFreqsByClass -> Float
evaluateAccuracy textSentiments freqs = numOfAccurate / fromIntegral (length textSentiments)
    where numOfAccurate = sum [1 | (sentiment, text) <- V.toList textSentiments, naiveBayesClassifier text freqs - sentiment <= 1]
```

**Python**: Explicitly loops through test data to count correct predictions.
```
def evaluate_accuracy(test_data: List[TextSentiment], freqs: WordFreqsByClass) -> float:
    correct = 0
    for sentiment, text in test_data:
        if abs(naive_bayes_classifier(text, freqs) - sentiment) <= 1:
            correct += 1
    return correct / len(test_data)
```

A loop iterates over each item, maintaining a mutable `correct` counter.

<br>

**Observations**

Haskell’s functional paradigm emphasizes composability, reusability, and immutability. The code is concise but might feel more abstract. Python’s imperative style offers step-by-step clarity but can become and error-prone for complex transformations.
Both programming styles are effective but suit different preferences and problem domains.
