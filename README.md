# ebookify
Create an eBook from a collection of documents using pandoc and
tex4ebook.

Currently the only backend supported is MongoDB.

# Set up

    (add-to-list 'load-path "/path/to/ebookify")
    (require 'ebookify-mode)


# Create an eBook

    (ebookify-create-ebook "Mon titre" "Moi" "00001, 00002")
