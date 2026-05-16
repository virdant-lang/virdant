/**
 * Add copy buttons to code blocks in Sphinx documentation
 */

document.addEventListener('DOMContentLoaded', function() {
    // Find all code blocks (highlight divs)
    const codeBlocks = document.querySelectorAll('div.highlight');

    codeBlocks.forEach(function(codeBlock) {
        // Create the copy button
        const copyButton = document.createElement('button');
        copyButton.className = 'copy-button';
        copyButton.type = 'button';
        copyButton.setAttribute('aria-label', 'Copy code to clipboard');
        copyButton.innerHTML = `
            <svg class="copy-icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="16" height="16">
                <path d="M16 1H4c-1.1 0-2 .9-2 2v14h2V3h12V1zm3 4H8c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h11c1.1 0 2-.9 2-2V7c0-1.1-.9-2-2-2zm0 16H8V7h11v14z"/>
            </svg>
            <svg class="check-icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="16" height="16" style="display: none;">
                <path d="M9 16.17L4.83 12l-1.42 1.41L9 19 21 7l-1.41-1.41z"/>
            </svg>
        `;

        // Position the button relative to the code block
        codeBlock.style.position = 'relative';

        // Insert the button as the first child of the code block
        codeBlock.insertBefore(copyButton, codeBlock.firstChild);

        // Add click event
        copyButton.addEventListener('click', function() {
            // Find the pre element within this code block
            const pre = codeBlock.querySelector('pre');
            if (!pre) return;

            // Get the text content, excluding line numbers if present
            let text;
            const hasLineNumbers = codeBlock.querySelector('.linenos');

            if (hasLineNumbers) {
                // Clone the pre element to avoid modifying the original
                const preClone = pre.cloneNode(true);
                // Remove all line number spans
                const lineNumberSpans = preClone.querySelectorAll('.linenos');
                lineNumberSpans.forEach(span => span.remove());
                text = preClone.textContent;
            } else {
                text = pre.textContent;
            }

            // Copy to clipboard
            navigator.clipboard.writeText(text).then(function() {
                // Show success feedback
                const copyIcon = copyButton.querySelector('.copy-icon');
                const checkIcon = copyButton.querySelector('.check-icon');

                copyIcon.style.display = 'none';
                checkIcon.style.display = 'inline';
                copyButton.classList.add('copied');

                // Reset after 2 seconds
                setTimeout(function() {
                    copyIcon.style.display = 'inline';
                    checkIcon.style.display = 'none';
                    copyButton.classList.remove('copied');
                }, 2000);
            }).catch(function(err) {
                console.error('Failed to copy text: ', err);
            });
        });
    });
});
