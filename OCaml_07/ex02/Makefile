SOURCES = people.ml dalek.ml doctor.ml main.ml
RESULT  = ex02
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