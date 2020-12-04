
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <utility>
#include <numeric>
#include <cctype>
#include <sstream>


std::vector<std::string> spell_check(std::string word, std::vector<std::string>& dictionary)
{
	struct {
		std::size_t edit_distance = -1;
		std::vector<std::string> list;
	} ReturnData;                                            //структура для формирования результатов выборки из словаря. Из функции возвращается RetData.list
	std::string word_as_is=word, dictionary_word_as_is;      //сохранение регистра слов "как есть"
	std::vector<std::vector<std::size_t>> edit_order_matrix; //вектор, хранящий матрицу для вычисления редакционного предписания
	
	/*так как по условию преобразование не может быть больше чем из двух действий 
	то выбираются слова, отличающиеся от проверяемого слова не более чем на два знака*/
	
	std::transform(word.begin(), word.end(), word.begin(), [](unsigned char c) { return std::tolower(c); });
	std::size_t max_word_length = word.length() + 2;
	std::size_t min_word_length = word.length() > 2 ? word.length() - 2 : 1;

   for (auto dictionary_word : dictionary) {
		dictionary_word_as_is = dictionary_word;
		std::transform(dictionary_word.begin(), dictionary_word.end(), dictionary_word.begin(), [](unsigned char c) { return std::tolower(c); });
   if(dictionary_word.size()>= min_word_length && dictionary_word.size()<= max_word_length){

	const std::size_t dictionary_word_size = dictionary_word.size();
	const std::size_t word_size = word.size();

	/*так как для реализации алгоритма Левенштейна (без транспозиции) нужно всего две строки (текущая и предыдущая) 
	то в роли этих строк выступают векторы upline и lowline */

	std::vector<std::size_t> upline,lowline;
	edit_order_matrix.clear();
	edit_order_matrix.shrink_to_fit();
	upline.resize(word_size + 1);
	edit_order_matrix.resize(dictionary_word.size()+1);
    std::iota(upline.begin(), upline.end(), 0);
	lowline = upline;

	for (std::size_t i = 0; i < dictionary_word_size; ++i)
	{
		lowline[0] = i + 1;
		edit_order_matrix[i].resize(word_size + 1);
		for (std::size_t j = 0; j < word_size; ++j)
		{
			auto delete_cost = upline[j + 1] + 1;
			auto insert_cost = lowline[j] + 1;
			auto substitution_cost = dictionary_word[i] == word[j] ? upline[j] : (upline[j] + 1);

			lowline[j + 1] = std::min({ delete_cost, insert_cost, substitution_cost });
		}

		edit_order_matrix[i] = upline;
		edit_order_matrix[i+1].resize(word_size + 1);
		edit_order_matrix[i+1] = lowline;
		std::swap(upline, lowline);
	}
	edit_order_matrix.shrink_to_fit();

	/*матрица создана, далее вычисление "редакционного предписания"*/

	std::size_t position_i = edit_order_matrix.size() - 1, position_j = edit_order_matrix[0].size() - 1, next_position_i = 0, next_position_j = 0;
	std::size_t  delete_operation = 0, insert_operation = 0, no_operation = 0;
	bool diagonal_move;
	bool double_operation = false;         // флаг "двух оперций" подряд (из условия)
	std::size_t previous_operation = -1;   //хранится "предыдущая операция"
	std::size_t edit_weight=0;
	
	if (edit_order_matrix[position_i][position_j] <= 2) {    // разрешено не более двух правок
		if (edit_order_matrix[position_i][position_j] != 0) {
	/* если количество правок  1 или 2 то требуется вычислить "вес" этих правок, если количество правок 0 то найдено точное совпадение и "вес"==0*/
			while (position_i >= 0 && position_j >= 0) {
				if (position_i == 0 && position_j == 0) { break; }

				/*
				так как по условию операция замены символов запрещена то по диагонали можно двигаться только если буквы на соответвующих позициях в словах совпадают
				иначе разрешены вставка/удаление
				*/
				if (position_i == 0 || position_j == 0) { diagonal_move = false; }//проверка можно ли двигаться по диагонали
				else { diagonal_move = dictionary_word[(edit_order_matrix[position_i - 1][0])] == word[(edit_order_matrix[0][position_j - 1])] ? 1 : 0; }
				if (diagonal_move) {              //если  буквы в сравниваемых словах в позиции i и j равны ничего не делать и перейти "по диагонали"
					no_operation++;
					position_i -= 1; position_j -= 1;
					previous_operation = 0;
				}
				else {                       //если буквы не равны перейти либо "по горизонтали" либо "по вертикали" выбрав меньшее значение

					next_position_j = (position_j == 0) ? -1 : edit_order_matrix[position_i][position_j - 1];    // при j==0 удалений больше нет
					next_position_i = (position_i == 0) ? -1 : edit_order_matrix[position_i - 1][position_j];    //при i==0 вставок больше нет

					if (next_position_j <= next_position_i) {
						delete_operation++; position_i = position_i; position_j -= 1;
						if (previous_operation == 1) { double_operation = true; break; }
						else { previous_operation = 1; }
					}
					else {
						insert_operation++; position_i -= 1; position_j = position_j;
						if (previous_operation == 2) { double_operation = true; break; }
						else { previous_operation = 2; }
					}
				}
			}
		}
		edit_weight = delete_operation + insert_operation; //вычисляем "вес" редакционного пердписания

		/*
		Если "вес" редакционного предписания равен 0 значит найдено точное совпадение
		Если "вес" больше двух то слово из словаря отбрасывается
		Дальнейший выбор в соостветсии с условием: если есть хотябы одно (или несколько) слов с одной операцией, слова с двумя операциями отбрасываются.
		*/
		if (!double_operation && edit_weight <= 2) {
			if (edit_weight == 0) { ReturnData.list.clear(); ReturnData.list.push_back(word_as_is); return ReturnData.list; }
			if (edit_weight == 1) { if (ReturnData.edit_distance == 1) { ReturnData.list.push_back(dictionary_word_as_is); } else { ReturnData.edit_distance = 1; ReturnData.list.clear(); ReturnData.list.push_back(dictionary_word_as_is); } }
			if (edit_weight == 2) { if (ReturnData.edit_distance != 1) { ReturnData.edit_distance = 2; ReturnData.list.push_back(dictionary_word_as_is); } }
		}
	}
	 }
	}
	return ReturnData.list;
}



int main() {
	

     std::vector<std::string> dictionary{};        //массив проверочных слов (словарь)
	 std::vector<std::string> dictionary_check{};  //массив слов для проверки
	 std::vector<std::string> spell_check_result;  //вектор получающий результат выполнения функции spell_check
	 std::string input_line;
	 bool dictionary_check_border = false;
	 std::string sub_line;


	 while (std::getline(std::cin, input_line))
	 {
		 if (input_line == "===") { if (dictionary_check_border) break; dictionary_check_border = !dictionary_check_border; continue; }
		 
		 if (!dictionary_check_border) {
			 if (!input_line.empty()) {
				 	 std::stringstream  data(input_line);
					 while (std::getline(data, sub_line, ' ')) { dictionary.push_back(sub_line); }
			  }
		 }
		 else {
			 if (!input_line.empty()) { dictionary_check.push_back(input_line); }
		 }
	 }
	 if (!dictionary.empty() && !dictionary_check.empty()) {

		 for (auto& word : dictionary_check) {
	 
	     std::stringstream  data(word);
		 while (std::getline(data, sub_line, ' ')) { 
			 spell_check_result = spell_check(sub_line, dictionary);
			 if (!spell_check_result.empty()) {
				if (spell_check_result.size() > 1) {
					std::cout << "{"; 
					for (int i = 0; i < spell_check_result.size(); ++i) {
						if (i == 0) { std::cout << spell_check_result[i]; }
						else {
							std::cout << " " << spell_check_result[i];
						}
					}
					std::cout << "}";
				}
				else {
					std::cout << spell_check_result[0];
				}
				std::cout << " ";
			 }
			 else {
				 std::cout << "{" << sub_line << "?} ";
			 }
		 }
		 std::cout << '\n';
	 
	  }
	 }
}