'use strict';

Array.prototype.slice.call(document.querySelector('#projects').children).forEach(function (project) {
    project.addEventListener('click', function () {
        var xhr = new XMLHttpRequest();
        xhr.open('GET', window.location.pathname + 'posts/' + project.textContent + '.md?_=' + new Date().getTime());
        xhr.onload = function () {
            if (xhr.readyState === 4 && xhr.status === 200) {
                console.log(xhr.responseText);
                document.querySelector('#post').innerHTML = marked(xhr.responseText);
            }
        };
        xhr.onerror = function (e) {
            console.error(xhr.statusText);
        };
        xhr.send(null);
    });
});