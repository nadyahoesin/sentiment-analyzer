# sentiment-analyzer

### Overview

Sentiment Analyzer is a web application written in Haskell that classifies the sentiment of text input. The application uses a Naive Bayes algorithm, trained on labeled data, to analyze text sentiment and classify it on a scale from strongly negative (0) to strongly positive (4). This application is publicly accessible at https://sentiment-analyzer-nadya-6ca2373a8d05.herokuapp.com.

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
