SOURCES = people.ml doctor.ml dalek.ml army.ml main.ml
RESULT = ex03
OCAMLOPT = ocamlopt

all: $(RESULT)

$(RESULT): $(SOURCES)
	$(OCAMLOPT) -o $@ $^

clean:
	rm -f *.cm* *.o

fclean: clean
	rm -f $(RESULT)

re: fclean all

.PHONY: all clean fclean re