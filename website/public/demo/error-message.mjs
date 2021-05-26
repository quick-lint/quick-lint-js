function showErrorMessageBox(mark, errorMessage) {
  const div = createErrorBox(mark, errorMessage, "code", "severity");
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
    `position: absolute; 
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

function assignBoxErrorMessage(shadowCodeInputElement, marks) {
  const elements = shadowCodeInputElement.querySelectorAll("mark");
  const errorMessages = marks.map(({ code, message, severity }, index) => ({
    code,
    message,
    severity,
    mark: elements[index],
  }));

  elements.forEach((mark) => {
    mark.addEventListener("onmousemove", (event) =>
      showErrorMessageBox(event, errorMessages)
    );
    mark.addEventListener("onmouseout", removeErrorMessageBox);
  });
}

function showErrorMessage(event) {
  const mousePositionX = event.clientX;
  const mousePositionY = event.clientY;
  const shadowInput = document.querySelector("#shadow-code-input");
  Array.from(shadowInput.querySelectorAll("mark")).every((mark) => {
    const { bottom, left, top, height, width } = mark.getBoundingClientRect();
    const topDownIn = bottom >= mousePositionY && mousePositionY >= top;
    const leftRightIn =
      mousePositionX >= left && mousePositionX <= left + width;
    if (topDownIn && leftRightIn) {
      const errorMessage = mark.getAttribute("data-message");
      showErrorMessageBox(mark, errorMessage);
      return false;
    }
    removeErrorMessageBox();
    return true;
  });
}

document.addEventListener("DOMContentLoaded", () => {
  document
    .querySelector("#code-input")
    .addEventListener("mousemove", showErrorMessage);
});
