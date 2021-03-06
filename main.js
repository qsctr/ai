'use strict';

var post = document.querySelector('#post');
var commentList = document.querySelector('#comment-list');

Array.prototype.slice.call(document.querySelector('#projects').children).forEach(function (project) {
    project.addEventListener('click', function () {
        var xhr = new XMLHttpRequest();
        xhr.open('GET', window.location.pathname + 'posts-html/' + project.textContent + '.html?_=' + new Date().getTime());
        xhr.onload = function () {
            if (xhr.readyState === 4 && xhr.status === 200) {
                post.innerHTML = xhr.responseText;
                loadComments(project.textContent);
                location.hash = '';
                location.hash = '#post';
            }
        };
        xhr.onerror = function () {
            post.innerHTML = 'Could not load post (' + xhr.statusText + ')';
        };
        xhr.send(null);
    });
});

function loadComments(postname) {
    document.querySelector('#postname').value = postname;
    document.querySelector('#comments-container').style.display = 'block';
    commentList.innerHTML = '';
    var xhr = new XMLHttpRequest();
    xhr.open('GET', 'https://getsimpleform.com/messages.json?api_token=a4e4a9772e551c53fdde1752d2fb54ce');
    xhr.onload = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            var comments = JSON.parse(xhr.responseText);
            comments.sort(function (a, b) {
                return new Date(a.created_at).getTime() - new Date(b.created_at).getTime();
            });
            comments.forEach(function (comment) {
                console.log(comment.created_at);
                if (comment.data.postname === postname) {
                    var nameEl = document.createElement('p');
                    nameEl.textContent = comment.data.name + ' at ' + new Date(comment.created_at) + ':';
                    commentList.appendChild(nameEl);
                    var commentEl = document.createElement('div');
                    commentEl.innerHTML = comment.data.comment;
                    commentEl.className = 'comment';
                    commentList.appendChild(commentEl);
                }
            });
        }
    };
    xhr.onerror = function () {
        commentList.innerHTML = "Could not load comments.";
    }
    xhr.send(null);
}
