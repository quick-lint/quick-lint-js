function showErrorMessageBox(mark) {
  const div = createErrorBox(
    mark,
    mark.attributes["data-message"].value,
    mark.attributes["data-code"].value,
    mark.attributes["data-severity"].value
  );
  let body = document.querySelector("body");
  body.appendChild(div);
}

function createErrorBox(markedElement, errorMessage, code, severity) {
  // TODO: Change background color based of the severity
  let div = document.createElement("div");
  const { bottom, left, height } = markedElement.getBoundingClientRect();
  div.setAttribute("id", "error-message-box");
  div.innerText = `${code} - ${errorMessage}`;
  div.setAttribute(
    "style",
    `position: fixed; 
    top: ${Math.trunc(bottom)}px; 
    left: ${Math.trunc(left)}px`
  );
  div.classList.add("error-box");
  return div;
}

function removeErrorMessageBox() {
  const errorMessagesBoxs = document.querySelectorAll("#error-message-box");
  errorMessagesBoxs.forEach((box) => box.remove());
}

function showErrorMessage(event) {
  const shadowInput = document.querySelector("#shadow-code-input");
  Array.from(shadowInput.querySelectorAll("mark")).every((mark) => {
    const markRect = mark.getBoundingClientRect();
    if (cursorOverMark(event.clientX, event.clientY, markRect)) {
      showErrorMessageBox(mark);
      return false;
    }
    removeErrorMessageBox();
    return true;
  });
}

function cursorOverMark(cursorPosX, cursorPosY, markRect) {
  const topDownIn = markRect.bottom >= cursorPosY && cursorPosY >= markRect.top;
  const leftRightIn =
    cursorPosX >= markRect.left && cursorPosX <= markRect.left + markRect.width;
  return topDownIn && leftRightIn;
}

document.addEventListener("DOMContentLoaded", () => {
  document
    .querySelector("#code-input")
    .addEventListener("mousemove", showErrorMessage);
});
