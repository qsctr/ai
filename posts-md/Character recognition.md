# Character recognition

See the source code on GitHub: https://github.com/qsctr/character-recognition

Use the program: https://qsctr.github.io/character-recognition (If there is an error dialog, use a newer browser. Chrome is recommended.)

## 4/6/2017

For this assignment we are supposed to train ten perceptrons corresponding to the digits 0 to 9, and then use them to classify a 5 by 7 pixel large input, for digit recognition. However, I decided to do character recognition instead, so it will work for letters as well, and with a higher resolution input. I will write it in HTML, CSS, and TypeScript, so it can run in a webpage.

Today I finished designing the UI for the program. It is divided into three main areas. The one on the left is the input box. The user can press a key on the keyboard to render that character into the box, or draw one themselves. The middle one is the output box. It shows what the computer thinks the input is. The one on the right has buttons that the user can press, such as for classification and training.

## 4/10/2017

Today I finished the project. Currently the program recognizes lowercase and uppercase alphabetical letters, numeric digits, and space. However, this is just an arbitrary choice, since any character that is renderable by the computer can be used. For example, I tried training it with some Chinese characters, and it worked.

How it works is that one perceptron for each character to be recognized is created. If you want an explanation about perceptrons you can read the previous post about the perceptron project. However, this project is in TypeScript instead of Haskell, but the concept of perceptrons is still the same. But instead of getting x and y coordinates as inputs, the perceptrons in this project gets the pixels of the input picture as inputs. Since the current size of the canvas is 50 by 50 pixels, each perceptron has 2500 inputs, instead of 2.

Then, a training set is generated. Currently, 100 training samples are generated for each character, so since there are 63 characters in total, 6300 training samples are generated in total. The first one of each is just the normal rendering of that character into the 50 by 50 resolution canvas. But the 99 remaining ones have noise added in them, so some pixels might be darker or lighter randomly.

After that, the perceptrons are trained. Each perceptron is given the training samples of its corresponding digit with a target output of 1. The training samples of the other digits are given with a target of 0. The order of this is randomized. The same training algorithm is used as the last project, but there are just more inputs in this one.

Finally, to classify the input characters, the array of perceptrons is traversed to find the first one that gives an output of 1 when activated with the input. The character corresponding to that perceptron is then rendered in the output box.

Initially, there was a problem where the UI would freeze up during long operations such as generating the training set or training the perceptrons. This was because JavaScript is single threaded and relies on an event loop in the same thread to handle events.

For generating the training set, which requires access to the DOM since it has to render the character into the canvas element, I solved this by changing the loop to use `setInterval` instead with an interval of 0 so the event loop can run between generating the training samples for each character.

For training the perceptrons and classifying, which don't require access to the DOM, I solved it by moving them into a web worker. Web workers are a way of achieving parallelism in JavaScript in web browsers. They don't have access to the DOM and can only communicate with the main thread by posting or receiving messages. So when the train button is pressed, the main thread generates the samples and passes them to the worker in a message. As the worker trains the perceptrons, it periodically sends back the progress of training so it can be displayed on the screen. Then, when the classify button is pressed, the main thread sends the input to the worker, who then sends back which character it was classified as.

Since there is more code than usual in this project, I did not put any code in this post. You can check out the full source code on GitHub.

## 4/11/2017

Today I added the ability to save the trained perceptron data to local storage, so they don't have to be trained every time the page is refreshed. The weights and threshold of each perceptron are passed to the main thread and saved to local storage.
