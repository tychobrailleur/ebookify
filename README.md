# ebooklify
Create an eBook from a collection of documents using pandoc and
tex4ebook.

Currently the only backend supported is MongoDB.

# Set up

    (add-to-list 'load-path "/path/to/ebooklify")
    (require 'ebooklify-mode)


# Create an eBook

    (ebooklify-create-ebook "Mon titre" "Moi" "00001, 00002")
