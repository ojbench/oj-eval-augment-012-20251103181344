/**
 * implement a container like std::linked_hashmap
 */
#ifndef SJTU_LINKEDHASHMAP_HPP
#define SJTU_LINKEDHASHMAP_HPP

// only for std::equal_to<T> and std::hash<T>
#include <functional>
#include <cstddef>
#include "utility.hpp"
#include "exceptions.hpp"

namespace sjtu {
    /**
     * In linked_hashmap, iteration ordering is differ from map,
     * which is the order in which keys were inserted into the map.
     * You should maintain a doubly-linked list running through all
     * of its entries to keep the correct iteration order.
     *
     * Note that insertion order is not affected if a key is re-inserted
     * into the map.
     */
    
template<
	class Key,
	class T,
	class Hash = std::hash<Key>,
	class Equal = std::equal_to<Key>
> class linked_hashmap {
public:
	/**
	 * the internal type of data.
	 * it should have a default constructor, a copy constructor.
	 * You can use sjtu::linked_hashmap as value_type by typedef.
	 */
	typedef pair<const Key, T> value_type;

private:
	struct Node {
		value_type *data;
		Node *prev;
		Node *next;

		Node() : data(nullptr), prev(nullptr), next(nullptr) {}
		Node(const value_type &val) : data(new value_type(val)), prev(nullptr), next(nullptr) {}
		~Node() {
			if (data) delete data;
		}
	};

	struct Bucket {
		Node *node;
		Bucket *next;

		Bucket() : node(nullptr), next(nullptr) {}
		Bucket(Node *n) : node(n), next(nullptr) {}
	};

	Bucket **table;
	size_t table_size;
	size_t element_count;
	Node *head;
	Node *tail;
	Hash hash_func;
	Equal equal_func;

	static const size_t INITIAL_SIZE = 16;
	static constexpr double LOAD_FACTOR = 0.75;

	void init_table(size_t size) {
		table_size = size;
		table = new Bucket*[table_size];
		for (size_t i = 0; i < table_size; ++i) {
			table[i] = nullptr;
		}
	}

	void clear_table() {
		if (table) {
			for (size_t i = 0; i < table_size; ++i) {
				Bucket *bucket = table[i];
				while (bucket) {
					Bucket *next = bucket->next;
					delete bucket;
					bucket = next;
				}
			}
			delete[] table;
			table = nullptr;
		}
	}

	void clear_list() {
		Node *current = head;
		while (current) {
			Node *next = current->next;
			delete current;
			current = next;
		}
		head = tail = nullptr;
	}

	void rehash() {
		size_t new_size = table_size * 2;
		Bucket **new_table = new Bucket*[new_size];
		for (size_t i = 0; i < new_size; ++i) {
			new_table[i] = nullptr;
		}

		Node *current = head;
		while (current) {
			size_t index = hash_func(current->data->first) % new_size;
			Bucket *new_bucket = new Bucket(current);
			new_bucket->next = new_table[index];
			new_table[index] = new_bucket;
			current = current->next;
		}

		clear_table();
		table = new_table;
		table_size = new_size;
	}

	Node* find_node(const Key &key) const {
		size_t index = hash_func(key) % table_size;
		Bucket *bucket = table[index];
		while (bucket) {
			if (equal_func(bucket->node->data->first, key)) {
				return bucket->node;
			}
			bucket = bucket->next;
		}
		return nullptr;
	}

public:
	/**
	 * see BidirectionalIterator at CppReference for help.
	 *
	 * if there is anything wrong throw invalid_iterator.
	 *     like it = linked_hashmap.begin(); --it;
	 *       or it = linked_hashmap.end(); ++end();
	 */
	class const_iterator;
	class iterator {
	private:
		/**
		 * TODO add data members
		 *   just add whatever you want.
		 */
		Node *node_ptr;
		const linked_hashmap *map_ptr;

		friend class linked_hashmap;
		friend class const_iterator;
	public:
		// The following code is written for the C++ type_traits library.
		// Type traits is a C++ feature for describing certain properties of a type.
		// For instance, for an iterator, iterator::value_type is the type that the
		// iterator points to.
		// STL algorithms and containers may use these type_traits (e.g. the following
		// typedef) to work properly.
		// See these websites for more information:
		// https://en.cppreference.com/w/cpp/header/type_traits
		// About value_type: https://blog.csdn.net/u014299153/article/details/72419713
		// About iterator_category: https://en.cppreference.com/w/cpp/iterator
		using difference_type = std::ptrdiff_t;
		using value_type = typename linked_hashmap::value_type;
		using pointer = value_type*;
		using reference = value_type&;
		using iterator_category = std::output_iterator_tag;


		iterator() : node_ptr(nullptr), map_ptr(nullptr) {}
		iterator(Node *node, const linked_hashmap *map) : node_ptr(node), map_ptr(map) {}
		iterator(const iterator &other) : node_ptr(other.node_ptr), map_ptr(other.map_ptr) {}
		/**
		 * TODO iter++
		 */
		iterator operator++(int) {
			if (!node_ptr) throw invalid_iterator();
			iterator temp = *this;
			node_ptr = node_ptr->next;
			return temp;
		}
		/**
		 * TODO ++iter
		 */
		iterator & operator++() {
			if (!node_ptr) throw invalid_iterator();
			node_ptr = node_ptr->next;
			return *this;
		}
		/**
		 * TODO iter--
		 */
		iterator operator--(int) {
			if (!map_ptr) throw invalid_iterator();
			if (!node_ptr) {
				if (!map_ptr->tail) throw invalid_iterator();
				iterator temp(map_ptr->tail, map_ptr);
				node_ptr = map_ptr->tail;
				return temp;
			}
			if (!node_ptr->prev) throw invalid_iterator();
			iterator temp = *this;
			node_ptr = node_ptr->prev;
			return temp;
		}
		/**
		 * TODO --iter
		 */
		iterator & operator--() {
			if (!map_ptr) throw invalid_iterator();
			if (!node_ptr) {
				if (!map_ptr->tail) throw invalid_iterator();
				node_ptr = map_ptr->tail;
				return *this;
			}
			if (!node_ptr->prev) throw invalid_iterator();
			node_ptr = node_ptr->prev;
			return *this;
		}
		/**
		 * a operator to check whether two iterators are same (pointing to the same memory).
		 */
		value_type & operator*() const {
			if (!node_ptr || !node_ptr->data) throw invalid_iterator();
			return *(node_ptr->data);
		}
		bool operator==(const iterator &rhs) const {
			return node_ptr == rhs.node_ptr && map_ptr == rhs.map_ptr;
		}
		bool operator==(const const_iterator &rhs) const {
			return node_ptr == rhs.node_ptr && map_ptr == rhs.map_ptr;
		}
		/**
		 * some other operator for iterator.
		 */
		bool operator!=(const iterator &rhs) const {
			return !(*this == rhs);
		}
		bool operator!=(const const_iterator &rhs) const {
			return !(*this == rhs);
		}

		/**
		 * for the support of it->first.
		 * See <http://kelvinh.github.io/blog/2013/11/20/overloading-of-member-access-operator-dash-greater-than-symbol-in-cpp/> for help.
		 */
		value_type* operator->() const noexcept {
			return node_ptr->data;
		}
	};

	class const_iterator {
		// it should has similar member method as iterator.
		//  and it should be able to construct from an iterator.
		private:
			// data members.
			Node *node_ptr;
			const linked_hashmap *map_ptr;

			friend class linked_hashmap;
		public:
			using difference_type = std::ptrdiff_t;
			using value_type = typename linked_hashmap::value_type;
			using pointer = value_type*;
			using reference = value_type&;
			using iterator_category = std::output_iterator_tag;

			const_iterator() : node_ptr(nullptr), map_ptr(nullptr) {}
			const_iterator(Node *node, const linked_hashmap *map) : node_ptr(node), map_ptr(map) {}
			const_iterator(const const_iterator &other) : node_ptr(other.node_ptr), map_ptr(other.map_ptr) {}
			const_iterator(const iterator &other) : node_ptr(other.node_ptr), map_ptr(other.map_ptr) {}

			const_iterator operator++(int) {
				if (!node_ptr) throw invalid_iterator();
				const_iterator temp = *this;
				node_ptr = node_ptr->next;
				return temp;
			}

			const_iterator & operator++() {
				if (!node_ptr) throw invalid_iterator();
				node_ptr = node_ptr->next;
				return *this;
			}

			const_iterator operator--(int) {
				if (!map_ptr) throw invalid_iterator();
				if (!node_ptr) {
					if (!map_ptr->tail) throw invalid_iterator();
					const_iterator temp(map_ptr->tail, map_ptr);
					node_ptr = map_ptr->tail;
					return temp;
				}
				if (!node_ptr->prev) throw invalid_iterator();
				const_iterator temp = *this;
				node_ptr = node_ptr->prev;
				return temp;
			}

			const_iterator & operator--() {
				if (!map_ptr) throw invalid_iterator();
				if (!node_ptr) {
					if (!map_ptr->tail) throw invalid_iterator();
					node_ptr = map_ptr->tail;
					return *this;
				}
				if (!node_ptr->prev) throw invalid_iterator();
				node_ptr = node_ptr->prev;
				return *this;
			}

			const value_type & operator*() const {
				if (!node_ptr || !node_ptr->data) throw invalid_iterator();
				return *(node_ptr->data);
			}

			bool operator==(const iterator &rhs) const {
				return node_ptr == rhs.node_ptr && map_ptr == rhs.map_ptr;
			}

			bool operator==(const const_iterator &rhs) const {
				return node_ptr == rhs.node_ptr && map_ptr == rhs.map_ptr;
			}

			bool operator!=(const iterator &rhs) const {
				return !(*this == rhs);
			}

			bool operator!=(const const_iterator &rhs) const {
				return !(*this == rhs);
			}

			const value_type* operator->() const noexcept {
				return node_ptr->data;
			}
	};

	/**
	 * TODO two constructors
	 */
	linked_hashmap() : table(nullptr), table_size(0), element_count(0), head(nullptr), tail(nullptr) {
		init_table(INITIAL_SIZE);
	}

	linked_hashmap(const linked_hashmap &other) : table(nullptr), table_size(0), element_count(0), head(nullptr), tail(nullptr) {
		init_table(other.table_size);
		Node *current = other.head;
		while (current) {
			insert(*(current->data));
			current = current->next;
		}
	}

	/**
	 * TODO assignment operator
	 */
	linked_hashmap & operator=(const linked_hashmap &other) {
		if (this == &other) return *this;
		clear();
		Node *current = other.head;
		while (current) {
			insert(*(current->data));
			current = current->next;
		}
		return *this;
	}

	/**
	 * TODO Destructors
	 */
	~linked_hashmap() {
		clear_list();
		clear_table();
	}

	/**
	 * TODO
	 * access specified element with bounds checking
	 * Returns a reference to the mapped value of the element with key equivalent to key.
	 * If no such element exists, an exception of type `index_out_of_bound'
	 */
	T & at(const Key &key) {
		Node *node = find_node(key);
		if (!node) throw index_out_of_bound();
		return node->data->second;
	}

	const T & at(const Key &key) const {
		Node *node = find_node(key);
		if (!node) throw index_out_of_bound();
		return node->data->second;
	}

	/**
	 * TODO
	 * access specified element
	 * Returns a reference to the value that is mapped to a key equivalent to key,
	 *   performing an insertion if such key does not already exist.
	 */
	T & operator[](const Key &key) {
		Node *node = find_node(key);
		if (node) return node->data->second;

		value_type new_pair(key, T());
		auto result = insert(new_pair);
		return result.first->second;
	}

	/**
	 * behave like at() throw index_out_of_bound if such key does not exist.
	 */
	const T & operator[](const Key &key) const {
		Node *node = find_node(key);
		if (!node) throw index_out_of_bound();
		return node->data->second;
	}

	/**
	 * return a iterator to the beginning
	 */
	iterator begin() {
		return iterator(head, this);
	}

	const_iterator cbegin() const {
		return const_iterator(head, this);
	}

	/**
	 * return a iterator to the end
	 * in fact, it returns past-the-end.
	 */
	iterator end() {
		return iterator(nullptr, this);
	}

	const_iterator cend() const {
		return const_iterator(nullptr, this);
	}

	/**
	 * checks whether the container is empty
	 * return true if empty, otherwise false.
	 */
	bool empty() const {
		return element_count == 0;
	}

	/**
	 * returns the number of elements.
	 */
	size_t size() const {
		return element_count;
	}

	/**
	 * clears the contents
	 */
	void clear() {
		clear_list();
		clear_table();
		init_table(INITIAL_SIZE);
		element_count = 0;
	}

	/**
	 * insert an element.
	 * return a pair, the first of the pair is
	 *   the iterator to the new element (or the element that prevented the insertion),
	 *   the second one is true if insert successfully, or false.
	 */
	pair<iterator, bool> insert(const value_type &value) {
		Node *existing = find_node(value.first);
		if (existing) {
			return pair<iterator, bool>(iterator(existing, this), false);
		}

		if (element_count >= table_size * LOAD_FACTOR) {
			rehash();
		}

		Node *new_node = new Node(value);

		// Add to linked list
		if (!head) {
			head = tail = new_node;
		} else {
			tail->next = new_node;
			new_node->prev = tail;
			tail = new_node;
		}

		// Add to hash table
		size_t index = hash_func(value.first) % table_size;
		Bucket *new_bucket = new Bucket(new_node);
		new_bucket->next = table[index];
		table[index] = new_bucket;

		element_count++;
		return pair<iterator, bool>(iterator(new_node, this), true);
	}

	/**
	 * erase the element at pos.
	 *
	 * throw if pos pointed to a bad element (pos == this->end() || pos points an element out of this)
	 */
	void erase(iterator pos) {
		if (!pos.node_ptr || pos.map_ptr != this) throw invalid_iterator();

		Node *node = pos.node_ptr;

		// Remove from hash table
		size_t index = hash_func(node->data->first) % table_size;
		Bucket *bucket = table[index];
		Bucket *prev_bucket = nullptr;

		while (bucket) {
			if (bucket->node == node) {
				if (prev_bucket) {
					prev_bucket->next = bucket->next;
				} else {
					table[index] = bucket->next;
				}
				delete bucket;
				break;
			}
			prev_bucket = bucket;
			bucket = bucket->next;
		}

		// Remove from linked list
		if (node->prev) {
			node->prev->next = node->next;
		} else {
			head = node->next;
		}

		if (node->next) {
			node->next->prev = node->prev;
		} else {
			tail = node->prev;
		}

		delete node;
		element_count--;
	}

	/**
	 * Returns the number of elements with key
	 *   that compares equivalent to the specified argument,
	 *   which is either 1 or 0
	 *     since this container does not allow duplicates.
	 */
	size_t count(const Key &key) const {
		return find_node(key) ? 1 : 0;
	}

	/**
	 * Finds an element with key equivalent to key.
	 * key value of the element to search for.
	 * Iterator to an element with key equivalent to key.
	 *   If no such element is found, past-the-end (see end()) iterator is returned.
	 */
	iterator find(const Key &key) {
		Node *node = find_node(key);
		if (node) return iterator(node, this);
		return end();
	}

	const_iterator find(const Key &key) const {
		Node *node = find_node(key);
		if (node) return const_iterator(node, this);
		return cend();
	}
};

}

#endif
