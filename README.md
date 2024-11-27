# sentiment-analyzer

### Overview

Sentiment Analyzer is a web application written in Haskell that classifies the sentiment of text input. The application uses a Naive Bayes algorithm, trained on labeled data, to analyze text sentiment and classify it on a scale from strongly negative (0) to strongly positive (4). This application is publicly accessible at https://sentiment-analyzer-nadya-6ca2373a8d05.herokuapp.com.

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
