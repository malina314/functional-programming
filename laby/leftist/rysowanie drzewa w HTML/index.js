console.log("hello");

function draw(input, handside) {
    let opensCount = 0;
    let left = '', right = '';
    let i = 4;
    for (; i < input.length; i++) {
        if (input[i] == '(') {
            opensCount++;
        } else if (input[i] == ')') {
            opensCount--;
        } else if (input[i] == ',' && opensCount == 0) {
            left = input.substring(4, i);
            break;
        }
    }
    let nodeValue = '';
    i += 2;
    while (0 <= input[i] && input[i] <=9) {
        nodeValue += input[i];
        i++;
    }
    nodeValue += '.' +  handside;

    for (; i < input.length; i++) {
        if (input[i] == 'E' || input[i] == 'N') {
            break;
        }
    }
    right = input.substring(i, input.length - 1);
    console.log(left);
    console.log(right);

    let Node = document.createElement('div')
    Node.classList.add('node');
    let divValue = document.createElement('div')
    divValue.classList.add('value');
    let children = document.createElement('div')
    children.classList.add('children');
    let circle = document.createElement('div')
    circle.classList.add('circle');
    circle.innerHTML = nodeValue;
    divValue.appendChild(circle);
    Node.appendChild(divValue);
    if (left && left[0] == 'E')
        children.appendChild(draw(left, 'L'));
    if (right && right[0] == 'E')
        children.appendChild(draw(right, 'R'));
    Node.appendChild(children);

    return Node;
}

function gen() {
    let input = document.querySelector('#pole').value;
    let el = draw(input, '');
    let plot = document.querySelector('#plot');
    plot.innerHTML = '';
    plot.appendChild(el);
}