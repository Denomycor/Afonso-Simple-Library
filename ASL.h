#pragma once
#include <type_traits>
#include <utility>
#include <stdexcept>
#include <iostream>
#include <chrono>

#define DEV 1

#if DEV
#define print(x) std::cout << x << std::endl;
//Inline definition changed from "inline function preferred" to "multiple defenitions"
#else
#error Changes to ASL.h not allowed
#endif


namespace asl {
    

    //Checks at compile time if all conditions are true.
    template<typename _st>
    constexpr bool conjunction(_st st) {
        return st;
    }
    
    //Checks at compile time if all conditions are true.
    template<typename _st, typename... Args>
    constexpr bool conjunction(_st st, Args...mul) {
        static_assert(std::is_convertible_v<_st, bool>, "'conjunction': all parameters must be booleans");
        return st and conjunction(mul...);
    }
    
    //Checks at compile time if at least one conditions is true.
    template<typename _st>
    constexpr bool disjunction(_st st) {
        return st;
    }

    //Checks at compile time if at least one condition is true.
    template<typename _st, typename... Args>
    constexpr bool disjunction(_st st, Args...mul) {
        static_assert(std::is_convertible_v<_st, bool>, "'disjunction': all parameters must be booleans");
        return st or disjunction(mul...);
    }

    //Casts before fowarding. 
    template<typename From, typename To>
    To forward_cast(From&& forw) { //copy elision
        if constexpr (std::is_reference_v<From>) { //is lvalue
            return static_cast<To>(forw);
        } else {                         //is rvalue
            return static_cast<To&&>(forw);
        }
    }

    //Checks whether two types are the same.
    template<typename T1, typename T2>
    constexpr bool is_same_v = false;
    template<typename T1>
    constexpr bool is_same_v<T1, T1> = true;

    //Enable if SFINAE.
    template<bool, typename ty = void>
    struct enable_if {
        using type = ty;
    };
    template<>
    struct enable_if<false> {};
    template<bool val, typename ty = void>
    using enable_if_t = typename enable_if<val, ty>::type;


    //Interface for a deleter Function/Lambda. Callable that takes no args.
    template<typename call>
    class deleter {
    private:
        call m_func;

    public:
        //Return Type.
        typedef decltype(m_func()) return_type;

        //Constructor.
        template<typename _call = call, typename = enable_if_t<std::is_invocable_v<_call>>>
        explicit deleter(const call& func)
            :m_func(func)
        {}

        //Operator Invocation.
        return_type operator()() const {
            return m_func();
        }

        //Gets the function.
        call get() const noexcept {
            return m_func;
        }

        //Returns true whether is std::nullptr_t or not.
        constexpr bool is_nullptr_t() const noexcept {
            return false;
        }

        //Returns true whether is nullpointing or not.
        bool is_nullptr() const noexcept {
            return m_func == nullptr;
        }
    };

    //Speciallization for nullptr deleters.
    template<>
    class deleter<std::nullptr_t>{
    public:
        explicit deleter(std::nullptr_t){}
        constexpr void operator()() const noexcept {}
        constexpr bool is_nullptr_t() const noexcept {return true;}
        constexpr bool is_nullptr() const noexcept {return true;}
    };
    deleter(std::nullptr_t)->deleter<std::nullptr_t>;

    //Test, benchmarking.
    class _NODISCARD time_this {
    private:
        std::chrono::time_point<std::chrono::steady_clock> start;
    public:
        time_this()
            :start(std::chrono::high_resolution_clock::now())
        {}
    
        ~time_this() {
            std::chrono::time_point<std::chrono::steady_clock>end(std::chrono::high_resolution_clock::now());
            std::chrono::duration<double> dur = end - start;
            std::cerr << dur.count() * 1000.0 << "ms" << std::endl;
        }
    };

    //Smart pointer. Safer.
    template<typename TYPE, bool is_arr = std::is_array_v<TYPE>>
    class [[nodiscard]] smart_ptr {
    private:
        
        template <typename, bool>
        friend class smart_ptr;  //All types of pointer can access all template instanciations of smart_ptr
        template <typename, bool>//So his members are private and accessible to derived classes without the
        friend class scoped_ptr; //protected access specifier, which werent accessible through handles of the
        template <typename, bool>//move ctor due of protected rules
        friend class safe_ptr; 
        template <typename, typename, bool>
        friend class lambda_ptr;
        
        typedef std::remove_extent_t<TYPE> gen; //gen is raw TYPE
        gen* m_data;

    public:
        //Constructor from raw pointer
        explicit smart_ptr(gen* heap_allocated_var)
            :m_data(heap_allocated_var)
        {}

        //Move Constructor
        smart_ptr(smart_ptr<TYPE, is_arr>&& move) noexcept
            :m_data(move.m_data)
        {
            //free(); //No need to free, new object isnt pointing anywhere
            //m_data = move.m_data;
            move.m_data = nullptr;
        }

        //Generalized Move Constructor
        template<typename MOVE, typename = std::enable_if_t<std::is_convertible_v<MOVE, TYPE>>>
        smart_ptr(smart_ptr<MOVE>&& move) noexcept
            :m_data(move.m_data)
        {
            //free(); //No need to free, new object isnt pointing anywhere
            //m_data = move.m_data;
            move.m_data = nullptr;
        }

        //No default constructor
        
        smart_ptr(const smart_ptr<TYPE, is_arr>&) = delete; //No copy allowed
        smart_ptr<TYPE, is_arr>& operator=(const smart_ptr<TYPE, is_arr>&) = delete; //No copy allowed
        //smart_ptr<TYPE, is_arr>& operator=(smart_ptr<TYPE, is_arr>&&); move assignment

        //Get raw pointer.
        gen* get() const & noexcept {
            return m_data;
        }

        //Dereference pointer.
        gen& operator*() const {
            return *m_data;
        }

        //Arrow operator.
        template<bool _arr = is_arr, typename = typename std::enable_if_t<!_arr>> //Sample of correct SFINAE
        gen* operator->() const {
            return m_data;
        }

        //Addressing operator.
        template<bool _arr = is_arr, typename = typename std::enable_if_t<_arr>> 
        gen& operator[](size_t index) const {
            return m_data[index];
        }

        //Returns true whether is nullpointing or not.
        bool is_nullptr() const noexcept {
            return m_data == nullptr;
        }

        //Converts to a std::unique_ptr.
        explicit operator std::unique_ptr<TYPE>() noexcept {
            TYPE* const holder = m_data;
            m_data = nullptr;
            return std::unique_ptr<TYPE>(holder);
        }

        //Delete memory.
        virtual void free() {
            if (!is_nullptr()) {
                if constexpr (is_arr)
                    delete[] m_data;
                else
                    delete m_data;
                m_data = nullptr;
            }
        }

        //Destructor
        virtual ~smart_ptr() {
            free();
        }
        
    };


    //Smart pointer. Versatile
    template <typename TYPE, bool is_arr = std::is_array<TYPE>::value>
    class [[nodiscard]] scoped_ptr : public smart_ptr<TYPE, is_arr> {
    private:
        typedef smart_ptr<TYPE, is_arr>::gen genb;
    
    public:
        //Constructor from raw pointer.
        explicit scoped_ptr(genb* heap_allocated_var)
            :smart_ptr<TYPE, is_arr>::smart_ptr(heap_allocated_var)
        {}

        //Move Constructor.
        scoped_ptr(scoped_ptr<TYPE, is_arr>&& move) noexcept
            :smart_ptr<TYPE, is_arr>::smart_ptr(std::move(move))
        {}

        //Generalized Move Constructor from a polymorfic rvalue.
        template<typename MOVE, typename = typename std::enable_if<std::is_convertible_v<MOVE, TYPE>>::type>
        scoped_ptr(scoped_ptr<MOVE>&& from) 
            : smart_ptr<TYPE, is_arr>::smart_ptr(std::move(from))
        {}

        scoped_ptr(const scoped_ptr<TYPE, is_arr>&) = delete; //No copy allowed
        scoped_ptr<TYPE, is_arr>& operator=(const scoped_ptr<TYPE, is_arr>&) = delete; //No copy allowed
        //scoped_ptr<TYPE, is_arr>& operator=(scoped_ptr<TYPE, is_arr>&&); move assignment

        //Delete a memory block and allocates again, returns true if memory is reseted.
        template<typename... Args, bool _arr = is_arr, typename = typename std::enable_if<!_arr>::type>
        void reset(Args&&... ctor);

        //Passes ownership of the resource to another pointer.
        bool passTo(scoped_ptr<TYPE, is_arr>& newOwner) {
            if (this->is_nullptr()) 
                return false;
            newOwner.smart_ptr<TYPE, is_arr>::free();
            newOwner.smart_ptr<TYPE, is_arr>::m_data = this->m_data;
            this->m_data = nullptr;
            return true;
        }
        
        //Receives another address.
        void receive(TYPE* new_handler) {
            smart_ptr<TYPE>::free();
            smart_ptr<TYPE, is_arr>::m_data = new_handler;
        }

        //Discard memory address, loses the pointer.
        void discard() noexcept {
            this->m_data = nullptr;
        }

        //Destructor
        /*~scoped_ptr() override {  //Use destructor from base class
            this->free();
        }*/

    };
 
    template<typename TYPE, bool is_arr>
    template<typename... Args, bool _arr, typename>
    inline void scoped_ptr<TYPE, is_arr>::reset(Args&&... ctor) {
        this->free();
        this->m_data = new TYPE(std::forward<Args>(ctor)...);
    }

    //A smart_ptr which calls a function at destruction.
    template<typename TYPE, bool is_arr = std::is_array_v<TYPE>>
    class [[nodiscard]] safe_ptr final : public smart_ptr<TYPE, is_arr> {
    private:
        typedef smart_ptr<TYPE, is_arr>::gen genb;
        typedef void(*deleterTy)(void); //Typedef for function pointer
        deleterTy dx_del;
    
    public:
        //Constructor.
        explicit safe_ptr(genb* heap_allocated_var, deleterTy dx_del)
            :smart_ptr<TYPE, is_arr>::smart_ptr(heap_allocated_var), dx_del(dx_del)
        {}

        //Move Constructor.
        safe_ptr(safe_ptr<TYPE, is_arr>&& move) noexcept
            :smart_ptr<TYPE, is_arr>::smart_ptr(std::move(move)), dx_del(move.dx_del)
        {}

        //Generalized Move Constructor.
        template<typename MOVE, typename = typename std::enable_if_t<std::is_convertible_v<MOVE, TYPE>>>
        safe_ptr(safe_ptr<MOVE>&& move) noexcept
            :smart_ptr<TYPE, is_arr>::smart_ptr(std::move(move)), dx_del(move.getDeleter())
        {}
        
        safe_ptr(const safe_ptr<TYPE, is_arr>&) = delete; //No copy allowed
        safe_ptr<TYPE, is_arr>& operator=(const safe_ptr<TYPE, is_arr>&) = delete; //No copy allowed

        //Gets the deleter of the object.
        deleterTy getDeleter() const noexcept {
            return dx_del;
        }

        //Delete memory.
        void free() override {
            if (!this->is_nullptr()) {
                if (dx_del != nullptr)
                    dx_del();
                if constexpr (is_arr)
                    delete[] smart_ptr<TYPE, is_arr>::m_data;
                else
                    delete smart_ptr<TYPE, is_arr>::m_data;
                smart_ptr<TYPE, is_arr>::m_data = nullptr;
            }
        }

        //Destructor
        ~safe_ptr() override {
            if(dx_del != nullptr && smart_ptr<TYPE, is_arr>::m_data != nullptr)
                dx_del();
        //When calling base class destructor, it will free the memory
        }
    };
    

    //Scoped_ptr with a lambda deleter.
    template<typename TYPE, typename del_ty, bool is_arr = std::is_array_v<TYPE>>
    class [[nodiscard]] lambda_ptr final : public scoped_ptr<TYPE, is_arr> {
    private:
        template<typename, typename, bool>
        friend class lambda_ptr;
        typedef std::remove_extent_t<TYPE> gen;
        asl::deleter<del_ty> m_del;

        //Destructor Implementation.
        template<typename nullpointer = del_ty>
        void des_impl() {
            if (!this->is_nullptr())
                if (m_del())
                    this->discard();
        }
        template<>
        constexpr void des_impl<std::nullptr_t>()noexcept {}

    public:
        //Constructor.
        explicit lambda_ptr(gen* data, const asl::deleter<del_ty>& deleter)
            :scoped_ptr<TYPE, is_arr>::scoped_ptr(data), m_del(deleter)
        {}

        //Move Constructor.
        lambda_ptr(lambda_ptr<TYPE, del_ty, is_arr>&& move) noexcept
            :scoped_ptr<TYPE, is_arr>::scoped_ptr(std::move(move)), m_del(move.m_del)
        {
           //move.discard();
        }

        //Generalized Move Constructor.
        template<typename MOVE, typename = std::enable_if_t<std::is_convertible_v<MOVE, TYPE>>>
        lambda_ptr(lambda_ptr<MOVE, del_ty>&& move) noexcept 
            :scoped_ptr<TYPE, is_arr>::scoped_ptr(std::move(move)), m_del(move.m_del)
        {
            //move.discard();
        }

        lambda_ptr(const lambda_ptr&) = delete; //No copy allowed
        lambda_ptr& operator=(const lambda_ptr&) = delete; //No copy allowed

        //Gets the deleter of the object for const objects.
        const deleter<del_ty>& getDeleter() const& noexcept {
            return m_del;
        }

        //Gets the deleter of the object.
        deleter<del_ty>& getDeleter()& noexcept {
            return m_del;
        }

        //Delete memory.
        void free() override {
            des_impl();
            if (!this->is_nullptr()) {
                if constexpr (is_arr)
                    delete[] smart_ptr<TYPE, is_arr>::m_data;
                else
                    delete smart_ptr<TYPE, is_arr>::m_data;
                smart_ptr<TYPE, is_arr>::m_data = nullptr;
            }
        }

        //Destructor
        ~lambda_ptr() override {
            des_impl();
        }

        //~lambda_ptr<TYPE, std::nullptr_t, is_arr>() override{}
    };

    //Template arguments deduction guide.
    template<typename TYPE, typename del_ty, bool is_arr = std::is_array_v<TYPE>>
    lambda_ptr(TYPE*, asl::deleter<del_ty>)->lambda_ptr<TYPE, del_ty, is_arr>;

    //Null deleter that does nothing.
    constexpr void nullFunc(...) {};
    static constexpr void(*nullDel)(...) = nullFunc;


    //Allocates memory and returns a pointer to it.
    template<typename TYPE, typename... Args, typename del_ty, typename = enable_if_t<std::is_invocable_v<del_ty, TYPE*>
        ||std::is_same_v<del_ty, std::nullptr_t>>>
    auto make_lambda(const del_ty& del, bool doesntDelete, Args&&... ctor) {
        static_assert(!std::is_array_v<TYPE>, "asl::make_lambda: invalid template arguments, try make_lambda_array");
        TYPE* holder = new TYPE(std::forward<Args>(ctor)...);
        if constexpr (std::is_same_v<del_ty, std::nullptr_t>)
            return lambda_ptr<TYPE, std::nullptr_t, false>(holder, asl::deleter(nullptr));
        else {
            auto lambda = [holder, &del, doesntDelete]() ->bool {del(holder); return doesntDelete; };
            return lambda_ptr<TYPE, decltype(lambda), false>(holder, asl::deleter(lambda));
        }
    }

    //Allocates memory, calls the constructor and returns a pointer to it.
    template<typename TYPE, typename... Args>
    safe_ptr<TYPE> make_safe(Args&&... ctor, void(*deleter)()) {
        static_assert(!std::is_array_v<TYPE>, "asl::make_safe: invalid template arguments, try make_safe_array");
        return safe_ptr<TYPE>(new TYPE(std::forward<Args>(ctor)...), deleter);
    }

    //Allocates memory, calls the constructor and returns a pointer to it.
    template<typename TYPE, typename ... Args>
    scoped_ptr<TYPE> make_scoped(Args&&... ctor) {
        static_assert(!std::is_array_v<TYPE>, "asl::make_scoped: invalid template arguments, try make_scoped_array");
        return scoped_ptr<TYPE>(new TYPE(std::forward<Args>(ctor)...));
    }

    //Allocates an array, and returns a pointer to it, requires default constructor.
    template<typename TYPE, typename del_ty, typename = std::enable_if_t<std::is_array_v<TYPE> && 
       (std::is_invocable_v<del_ty, std::remove_extent_t<TYPE>*> || std::is_same_v < del_ty, std::nullptr_t>)>>
    auto make_lambda_array(size_t size, const del_ty& del, bool doesntDel) {
        typedef std::remove_extent_t<TYPE> gen;
        static_assert(std::is_default_constructible_v<gen>, "Referenced type is not default constructible");
        gen* holder = new gen[size];
        if constexpr (std::is_same_v < del_ty, std::nullptr_t>)
            return lambda_ptr<TYPE, std::nullptr_t, true>(holder, deleter(nullptr));
        else {
            auto lambda = [holder, &del, doesntDel]() {del(holder); return doesntDel; };
            return lambda_ptr<TYPE, decltype(lambda), true>(holder, deleter(lambda));
        }
    }

    //Allocates an array, and returns a pointer to it, requires default constructor.
    template<typename TYPE, typename = typename std::enable_if_t<std::is_array_v<TYPE>>>
    safe_ptr<TYPE> make_safe_array(size_t size, void(*deleter)() =nullptr) {
        static_assert(std::is_default_constructible_v<std::remove_extent_t<TYPE>>, "Referenced type is not default constructible");
        return safe_ptr<TYPE>(new std::remove_extent_t<TYPE>[size], deleter);
    }

    //Allocates an array, and returns a pointer to it, requires default constructor.
    template<typename TYPE, typename = typename std::enable_if_t<std::is_array_v<TYPE>>>
    scoped_ptr<TYPE> make_scoped_array(size_t size) {
        static_assert(std::is_default_constructible_v<std::remove_extent_t<TYPE>>, "Referenced type is not default constructible");
        return scoped_ptr<TYPE>(new std::remove_extent_t<TYPE>[size]);
    }

    //Allocates an array, and returns a pointer to it.
    template<typename TYPE, typename del_ty, typename... Args, typename = std::enable_if_t<std::is_array_v<TYPE>
        && (std::is_invocable_v<del_ty, std::remove_extent_t<TYPE>*> || std::is_same_v<del_ty, std::nullptr_t>)
        && asl::conjunction(std::is_array_v<TYPE>, std::is_convertible_v<Args, std::remove_extent_t<TYPE>>...)>>
    auto make_lambda_array(const del_ty& del, bool doesntDel, Args&&... ctor) {
        typedef std::remove_extent_t<TYPE> gen;
        gen* holder = new gen[sizeof...(Args)]{ std::forward<Args>(ctor)... };
        if constexpr (std::is_same_v<del_ty, std::nullptr_t>)
            return lambda_ptr<TYPE, std::nullptr_t, true>(holder, deleter(nullptr));
        else {
            auto lambda = [holder, &del, doesntDel]() {del(holder); return doesntDel; };
            return lambda_ptr<TYPE, decltype(lambda), true>(holder, deleter(lambda));
        }
    }

    //Allocates an array, and returns a pointer to it.
    template<typename TYPE, typename... Args, typename = typename std::enable_if_t<asl::conjunction(
        std::is_array_v<TYPE>, std::is_convertible_v<Args, std::remove_extent_t<TYPE>>...)>>
    safe_ptr<TYPE> make_safe_array(void(*deleter)(), Args&&... mul) {
        return safe_ptr<TYPE>(new std::remove_extent_t<TYPE>[sizeof...(Args)]{std::forward<Args>(mul)... }, deleter);
    }

    //Allocates an array, and returns a pointer to it.
    template<typename TYPE, typename... Args, typename = typename std::enable_if_t<asl::conjunction(
        std::is_array_v<TYPE>,std::is_convertible_v<Args, std::remove_extent_t<TYPE>>...)>>
    scoped_ptr<TYPE> make_scoped_array(Args&&... mul) {
        return scoped_ptr<TYPE>(new std::remove_extent_t<TYPE>[sizeof...(Args)]{std::forward<Args>(mul)...});
    }



    //Array object, stack allocated
    template <typename gen, size_t _size>
    class array final {
    private:
        gen m_buffer[_size];

    public:
        //Constructor.
        template<typename... Args, typename = std::enable_if_t<
            std::is_convertible_v<std::common_type_t<Args...>, gen> && sizeof...(Args)<=_size > >
        array(Args&& ... mul)
            : m_buffer{ asl::forward_cast<Args, gen>(std::forward<Args>(mul))... }
        {}
        
        //Default constructor.
        array() = default;

        //Move constructor
        array(array<gen, _size>&& move) noexcept
            : m_buffer(move.m_buffer)
        {
            move.m_buffer = nullptr;
        }
        
        //Copy constructor.
        array(const array<gen, _size>& copy){ //Default constructor and copy assignment instead
            for (int i = 0; i < _size; i++) { //of copy constructor, not optimized
                m_buffer[i] = copy[i];
            }
        }

        //Operator *.
        const gen& operator*() & {
            return *m_buffer;
        }

        //Operator *.
        const gen& operator*() const & {
            return *m_buffer;
        }

        //Operator [].
        gen& operator[](size_t index) {
            return m_buffer[index];
        }
        
        //Operator [].
        const gen& operator[](size_t index) const {	 
            return m_buffer[index];
        }

        //Returns pointer to start of array;
        gen* get() & noexcept { 
            return m_buffer; //Rvalue cant call this function, getting memory address
                             //of the internal array will dangle since obj is a rvalue
        }

        //Returns pointer to start of array;               
        const gen* get() const & noexcept  { 
            return m_buffer;
        }

        //Gets the array size.
        constexpr size_t size() const noexcept {
            return _size;
        }

        //Enum having the size.
        enum {
            length = _size
            //java like access to array length
        };

        //Checks whether the array is empty.
        bool is_empty() const noexcept {
            return m_buffer == nullptr;
        }

        //Fills the array with a given a value.
        void fill(const gen& _fill) {
            for (gen& i : m_buffer) {
                i = _fill;
            }
        }

        //Support for range based for-loop.
        gen* begin() & {
            return m_buffer;
        }

        //Support for range based for-loop.
        gen* end() & {
            return m_buffer+_size;
        }

    };

    //No implementation for 0 sized arrays.
    template<typename gen>
    class array<gen, 0> final{};

    //template type deduction guide
    template<typename ... Init>
    array(Init...)->array<typename std::common_type<Init...>::type, sizeof...(Init)>;
    
    
    //Tracks changes on objects.
    template<typename TYPE>
    class observer final {
    private:
        
        TYPE* m_tracking; //Tracking data

        union { //Backup data
            TYPE m_backup;
            unsigned char m_as_char[sizeof(TYPE)];
        };

        //Remakes backup of the tracked object
        void makeBackup() {
            memcpy(&m_backup, m_tracking, sizeof(TYPE));
        }

    public:

        //Constructor.
        explicit observer(TYPE* looking)
            :m_tracking(looking), m_backup(*looking)
        {}

        //Copy Constructor.
        explicit observer(const observer<TYPE>& copy)
            :m_tracking(copy.get()), m_backup(copy.getBackup())
        {}

        //Copy Assignment.
        observer<TYPE>& operator=(const observer<TYPE>& copy) {
            m_tracking = copy.get();
            m_backup = copy.getBackup();
            return *this;
        }

        //No distinction from copy to moving

        //Gets size of tracking object.
        constexpr size_t getSize() const noexcept {
            return sizeof(TYPE);
        }

        //Checks whether the tracked object was changed.
        bool wasChanged() const {
            const unsigned char* const temp = reinterpret_cast<const unsigned char*>(m_tracking); //Is it ub??
            for (int i = 0; i < sizeof(TYPE); i++) {
                if (m_as_char[i] != *(temp + i)) return true;
            }
            return false;
        }

        //Restores any changes to the tracked object.
        void restore() const {
            static_assert(!std::is_const_v<TYPE>, "Cannot restore const objects");
            (*m_tracking) = m_backup;
        }

        //Returns the object being tracked.
        TYPE* get() const noexcept {
            return m_tracking;
        }

        //Gets the backup.
        const TYPE& getBackup() const noexcept {
            return m_backup;
        }

        //Sets the backup.
        void setBackup(const TYPE& backup) {
            m_backup = backup;
        }

        //Destructor
        ~observer(){} //Otherwise destructor is implicitly deleted, because has a member union

    };

    //Bubble sorts any container with operator[].
    template<typename _base, typename _alg>
    void bubble_sort(_base& toSort, size_t length, const _alg& func) {
        bool again;
        do {
            again = false;
            for (size_t i = 0; i < length - 1; i++) {
                if (func(toSort[i], toSort[i + 1])) {
                    std::swap(toSort[i], toSort[i + 1]);
                    again = true;
                }
            }
        } while (again);
    }

    //Class to represent a node of a list.
    template <typename type>
    class node {
    private:

        //Reset function can access protected constructor 
        friend class scoped_ptr<node<type>>;
        template<typename>
        friend class chain_list;
        template<typename>
        friend class modular_list;

        type m_elem;
        scoped_ptr<node<type>> m_next;

        //Constructor
        template<typename... Args>
        node(Args&&... ctor) 
            :m_elem(std::forward<Args>(ctor)...), m_next(nullptr)
        {}

        //Default Constructor
        node()
            :m_elem(), m_next(nullptr)
        {}

        //Gets the next node without checking for out_of_rage.
        node<type>& uncontrolled_dive() const {
            return *m_next;
        }

    public:
        //Gets the value of the current Node.
        type& value() & {
            return m_elem;
        }

        //Gets the next node.
        node<type>& dive() {
            try {
                if (m_next.is_nullptr()) 
                    throw std::out_of_range("Exception in node, trying to access "
                    "memory out of range\n");
                return *m_next;
            } catch (const std::out_of_range& err) {
                std::cerr << err.what();
                std::terminate();
            }
        }

    };

    //A class representing the head of a chain list, such head is immutable.
    template <typename type> 
    class [[nodiscard]] chain_list final : public node<type> {
    private:
        using baseClass = node<type>; //To access templated base class
        mutable size_t m_depth;

    public:
        //Constructor.
        template<typename... Args>
        explicit chain_list(Args&&... ctor) 
            :node<type>::node(std::forward<Args>(ctor)...), m_depth(0)
        {}

        //Default constructor.
        chain_list()
            :m_depth(0)
        {}

        //Inserts a node at the end of the list.
        template <typename... Args>
        node<type>& append(Args&&... ctor) {
            tail().m_next.reset(std::forward<Args>(ctor)...);
            m_depth++;
            return *baseClass::m_next;
        }

        //Makes n dives.
        node<type>& dive(size_t depth) {
            try {
                if (depth > m_depth) 
                    throw std::out_of_range("Exception in chain_list, trying to access memory out of range\n");
                
                node<type>* ptr = this;
                for (unsigned int i = 0; i < depth; i++) {
                    ptr = &ptr->uncontrolled_dive();
                }
                return *ptr;
            }
            catch(const std::out_of_range& err){
                std::cerr << err.what();
                std::terminate();
            }
        }

        //Gets the last node.
        node<type>& tail() {
            return dive(m_depth);
        }

        //Addressing operator.
        type& operator[](size_t index) {
            return dive(index).m_elem;
        }

        //Gets the length of the list.
        size_t size() const noexcept {
            return m_depth+1;
        }

        //Inserts a new node in the given index.
        template <typename... Args>
        node<type>& insert(size_t index, Args&&... ctor) {
            try {
                if (index == 0) 
                    throw std::exception("Exception in chain_list, cant insert "
                    "before the head of the list\n");

                if (index - 1 >= m_depth) {
                    return append();
                }
                else {
                    scoped_ptr<node<type>> new_node(new node<type>(std::forward<Args>(ctor)...));
                    node<type>& before = dive(index - 1);
                    before.m_next.passTo(new_node->m_next);
                    before.m_next.receive(new_node.get());
                    new_node.discard();
                    m_depth++;
                    return before.uncontrolled_dive();
                }

            } catch (const std::exception & err) {
                std::cerr << err.what();
                std::terminate();
            }
            
        }

        //Takes the last node out of the list.
        void pop() {
            dive(m_depth - 1).m_next.free();
            m_depth--;
        }

        //Removes a node at the given index. 
        void remove(size_t index) {
            try {
                if (index == 0) throw std::exception("Exception in linked_list,"
                    "cant remove the head of the list\n");

                if (index >= m_depth) {
                    pop();
                }
                else {
                    node<type>& before = dive(index - 1);
                    node<type>* to_delete = &before.uncontrolled_dive();
                    before.m_next.discard();
                    to_delete->m_next.passTo(before.m_next);
                    delete to_delete;
                    m_depth--;
                }

            } catch (const std::exception & err) {
                std::cerr << err.what();
                std::terminate();
            }
        }
        
        //Moves a node to a new position in the list.
        void move(size_t from, size_t to) {
            try {
                if (from == 0 || to == 0) throw std::exception("Exception in linked_list,"
                    "cant move the head of the list\n");

                node<type>& beforeFrom = dive(from - 1);
                node<type>* to_move = &beforeFrom.uncontrolled_dive();
                beforeFrom.m_next.discard();
                to_move->m_next.passTo(beforeFrom.m_next);
                node<type>& beforeTo = dive(to - 1);
                beforeTo.m_next.passTo(to_move->m_next);
                beforeTo.m_next.receive(to_move);

            } catch (const std::exception& err){
                    std::cerr << err.what();
                    std::terminate();
            }
        }

    };
  

    //A class representing a linked list.
    template <typename type>
    class [[nodiscard]] modular_list final {
    private:
        scoped_ptr<node<type>> m_start;
        mutable size_t m_depth;

    public:
        //Constructor
        template<typename... Args>
        explicit modular_list(Args&&... ctor)
            :m_start(new node<type>(std::forward<Args>(ctor)...)), m_depth(0)
        {}

        //Gets the value of the current Node.
        type& value()& {
            return m_start->m_elem;
        }

        //Gets the next node.
        node<type>& dive() {
            try {
                if (m_start->m_next.is_nullptr())
                    throw std::out_of_range("Exception in modular_list, trying to access memory out of range\n");
                return m_start->uncontrolled_dive();
            }
            catch (const std::out_of_range & err) {
                std::cerr << err.what();
                std::terminate();
            }
        }

        ////Makes n dives.
        node<type>& dive(size_t where) {
            try {
                if (where > m_depth)
                    throw std::out_of_range("Exception in modular_list, trying to access memory out of range\n");
                
                node<type>* ptr = m_start.get();
                for (; where > 0; where--) {
                    ptr = &ptr->uncontrolled_dive();
                }
                return *ptr;

            } catch (const std::out_of_range& err){
                std::cerr << err.what();
                std::terminate();
            }
            
        }

        //Inserts a node at the end of the list.
        template<typename... Args>
        node<type>& append(Args&&... ctor) {
            node<type>& before = dive(m_depth);
            before.m_next.reset(std::forward<Args>(ctor)...);
            m_depth++;
            return before.uncontrolled_dive();
        }

        //Gets the length of the list.
        size_t size() const noexcept {
            return m_depth + 1;
        }

        //Addressing operator.
        type& operator[](size_t where) {
            return dive(where).m_elem;
        }

        //Gets the last node.
        node<type>& tail() {
            return dive(m_depth);
        }
        
        //Takes the last node out of the list.
        void pop() {
            dive(m_depth-- - 1).m_next.free();
        }

        //Inserts a new node in the given index.
        template<typename... Args>
        node<type>& insert(size_t where, Args&&... ctor) {
            if (where == m_depth+1) 
                return append();
            node<type>* const to_insert(new node<type>(std::forward<Args>(ctor)...));
            scoped_ptr<node<type>>* before;
            if (where == 0)
                before = &this->m_start;
            else
                before = &dive(where - 1).m_next;
            before->passTo(to_insert->m_next);
            before->receive(to_insert);
            m_depth++;
            return *to_insert;
        }

        //Removes a node at the given index. 
        void remove(size_t where) {
            if (where == m_depth) {
                pop(); return;
            }
            scoped_ptr<node<type>>* before;
            if (where == 0)
                before = &this->m_start;
            else
                before = &dive(where - 1).m_next;
            scoped_ptr<node<type>> to_del(before->get());
            before->discard();
            to_del->m_next.passTo(*before);
            m_depth--;
        }

        //TODO, UNIMPLEMENTED
        void move(size_t from, size_t to) {

        }
    };
    


    //Pair having two variables.
    template<typename ty>
    class pair {
    private:
        ty m_first;
        ty m_second;

    public:
        //Constructor.
        pair(const ty& first, const ty& second)
            :m_first(first), m_second(second)
        {}

        //Copy Construcor.
        pair(const pair<ty>& copy)
            :m_first(copy.m_first), m_second(copy.m_second)
        {}

        //Generalized Copy Constructor.
        template<typename Copy, typename = typename std::enable_if_t<std::is_convertible_v<Copy, ty>>>
        pair(const pair<Copy>& copy)
            :m_first(copy.getFirst()), m_second(copy.getSecond())
        {}

        //Copy Assignment.
        pair<ty>& operator=(const pair<ty>& copy) noexcept {
            m_first = copy.m_first;
            m_second = copy.m_second;
            return *this;
        }

        //Generalized Copy Assignment. 
        template<typename Copy, typename = typename std::enable_if_t<std::is_convertible_v<Copy, ty>>>
        pair<ty>& operator=(const pair<Copy>& copy) noexcept {
            m_first = copy.getFirst();
            m_second = copy.getSecond();
            return *this;
        }

        //Default Constructor.
        pair() = default;

        //Setter First Elem.
        void setFirst(const ty& first) noexcept {
            m_first = first;
        }
        
        //Setter Second Elem.
        void setSecond(const ty& second) noexcept {
            m_second = second;
        }

        //Getter Fist Elem.
        ty getFirst() const noexcept {
            return m_first;
        }

        //Getter First Elem.
        ty getSecond() const noexcept {
            return m_second;
        }

        //Swap member variables.
        void swap() noexcept {
            //ty temp(m_first);
            //m_first = m_second;
            //m_second = m_first;
            std::swap<ty>(m_first, m_second);
        }
    };

    //Class ternary operator.
    template<typename type>
    class ternary : private pair<type> {
    public:
        //Constructor.
        explicit ternary(const type& if_true, const type& if_false)
            :pair<type>::pair(if_true, if_false)
        {}

        //Copy Constructor.
        ternary(const ternary<type>& copy)
            :pair<type>::pair(copy)
        {}

        //Default constructor.
        ternary()
            :pair<type>::pair()
        {}

        //Copy Assignment.
        ternary<type>& operator=(const ternary<type>& copy) noexcept {
            this->pair<type>::operator=(copy);
            return *this;
        }

        //Get value if true.
        type getIfTrue() const noexcept{
            return this->getFirst();
        }

        //Get value if false.
        type getIfFalse() const noexcept {
            return this->getSecond();
        }

        //Set value if true.
        void setIfTrue(const type& if_true) noexcept {
            this->setFirst(if_true);
        }

        //Set value if false.
        void setIfFalse(const type& if_false) noexcept {
            this->setSecond(if_false);
        }

        //Operator ?.
        type operator()(bool expression) const noexcept {
            if (expression)
                return this->getFirst();
            else
                return this->getSecond();
        }

        //Make private inherited swap function public.
        using pair<type>::swap;
    };


    //Compares memory from two buffers.
    bool memcmp(const void* first, const void* second, size_t size) {
        const char* _first = static_cast<const char*>(first);
        const char* _second = static_cast<const char*>(second);
        for (unsigned int i = 0; i < size; i++) {
            if (_first[i] != _second[i]) return false;
        }
        return true;
    }

    //Represent a hour and a minute.
    template<bool auto_adjust = true>
    class time {
    private:
        mutable unsigned int m_hour, m_min;

    public:
        //Default constructor.
        time()
            :m_hour(0), m_min(0)
        {}

        //Constructor.
        time(int hour, int minute)
            :m_hour(hour), m_min(minute)
        {
            if constexpr (auto_adjust) adjust();
        }

        //Constructor from decimal.
        time(float frac) {
            m_hour = static_cast<int>(frac);
            m_min = static_cast<int>((frac - m_hour) * 60);
        }

        //Copy constructor
        template<bool _adjust>
        time(const time<_adjust>& copy)
            :m_hour(copy.gHour()), m_min(copy.gMinute())
        {
            if constexpr (auto_adjust) adjust();
        } 

        //Copy assignment
        template<bool _adjust>
        time<auto_adjust>& operator=(const time<_adjust>& copy) {
            m_hour = copy.gHour();
            m_min = copy.gMinute();
            if constexpr (auto_adjust) adjust();
            return *this;
        }

        //Getter hour.
        int gHour() const noexcept {
            return m_hour;
        }

        //Getter minute.
        int gMinute() const noexcept {
            return m_min;
        }

        //Setter hour.
        void sHour(int hour) {
            m_hour = hour;
        }

        //Setter minute.
        void sMinute(int minute) {
            m_min = minute;
            if constexpr (auto_adjust) adjust();
        }

        //Explicit conversion to float.
        explicit operator float() const noexcept {
            return m_hour + (static_cast<float>(m_min) / 60);
        }
        
        //Operator +.
        const time operator+(const time& rhs) const noexcept {
            time<auto_adjust> temp(this->operator float() + rhs.operator float());
            if constexpr (auto_adjust) temp.adjust();
            return temp;
        }

        //Operator -.
        const time operator-(const time& rhs) const noexcept {
            time<auto_adjust> temp(this->operator float() - rhs.operator float());
            if constexpr (auto_adjust) temp.adjust();
            return temp;
        }

        //Operator <.
        const bool operator<(const time& rhs) const noexcept {
            return (this->operator float() < rhs.operator float());
        }

        //Operator >=.
        const bool operator>=(const time& rhs) const noexcept {
            return !operator<(rhs);
        }

        //Operator >.
        const bool operator>(const time& rhs) const noexcept {
            return (this->operator float() > rhs.operator float());
        }

        //Operator <=.
        const bool operator<=(const time& rhs) const noexcept {
            return !operator>(rhs);
        }

        //Operator ==
        const bool operator==(const time& rhs) const noexcept {
            return this->operator float() == rhs.operator float();
        }

        //Operator !=
        const bool operator!=(const time& rhs) const noexcept {
            return this->operator float() != rhs.operator float();
        }

        //Asserts time in clock.
        void adjust() const noexcept {
            m_hour += m_min / 60;
            m_min %= 60;
        }

    };
    template<bool adjust>
    std::ostream& operator<<(std::ostream& stream, const asl::time<adjust>& out) {
        stream << out.gHour() << 'h' << out.gMinute();
        return stream;
    }

    //Literal operator for time.
    time<true> operator"" _t(long double cl) {
        return time(static_cast<float>(cl));
    }

#if DEV
    //Logs Object operations to the console.
    struct log_point {
        int x, y;
        log_point(int px, int py)
            :x(px), y(py)
        {
            std::cout << "2 ctor\n"; 
        }

        log_point(log_point&& obj) noexcept
            :x(obj.x), y(obj.y)
        {
            std::cout << "2 move ctor\n"; 
        }

        log_point()
            : x(3), y(3)
        {
            std::cout << "2 def ctor\n";
        }

        log_point(const log_point& copy)
            :x(copy.x), y(copy.y)
        {
            std::cout << "2 copy ctor\n"; 
        }

        log_point& operator= (const log_point& copy) {
            x = copy.x;
            y = copy.y;
            std::cout << "2 copy assig\n";
            return *this;
        }
        virtual ~log_point() {
            std::cout << "2 dtor\n";
        }

        void printf() const {
            std::cout << x << ", " << y << '\n';
        }
    };
    struct log_point3 : public log_point {
        int z;
        log_point3(int px, int py, int pz)
            :log_point(px, py), z(pz)
        {
            std::cout << "3 ctor\n";
        }

        log_point3(log_point3&& rvalue) noexcept
            :log_point(rvalue.x, rvalue.y), z(rvalue.z)
        {
            std::cout << "3 move ctor\n";
        }

        log_point3()
            :log_point(), z(3)
        {
            std::cout << "3 def ctor\n";
        }

        log_point3(const log_point3& copy)
            :log_point(copy.x, copy.y), z(copy.z)
        {
            std::cout << "3 copy ctor\n"; 
        }

        log_point3& operator= (const log_point3& copy) {
            log_point::operator=(copy);
            z = copy.z;
            std::cout << "3 copy assig\n";
            return *this;
        }

        ~log_point3() {
            std::cout << "3 dtor\n";
        }
    };
#endif
    /*
    * Copyright (c) by A.R. Esteves. All rights reserved.
    * Project started: 17, November, 2019
    * Last edit: 25, December, 2019 V00.10.00
    * LEI - 2019 FCUL
    */
}


//Keywords:
//const, friend, virtual, override, constexpr, noexcept, explicit, static, extern, union, inline, mutable, final
//typename, typedef, template, decltype, using, namespace
//local_thread, volatile

//NOTE: Despite being being the same class template: scoped_ptr<int>, scoped_ptr<float>, when generating those classes,
//they will be independent and have no access to each other private members.

//NOTE: Default constructors can not initialize member variables, despide default constructing member obj
//when no member initializer list is provided

//NOTE: Explicit constructor disable implicit conversions to that type, and disable conversion constructors



//NOTE: lvalues can be converted to rvalues
//return var can call copy constructor if it cant call move constructor
//return value makes copy elision of same type as the return states

//if cant call move constructors/assignment compiler will try to call copy constructors/assignment

//The compiler generates the move constructor if:
/*there is no user-declared copy constructor, and
there is no user - declared copy assignment operator,and
there is no user - declared move assignment operator and
there is no user - declared destructor,
it is not marked deleted,
and all membersand bases are moveable*/

/* Protected members are accessible
1) by the members and friends of Base
2) by the members and friends (until C++17) of any class derived from Base,
but only when operating on an object or handler of a type that is derived from Base (including this pointer)

*/

/*{
auto&& __range = range_expression;
auto __begin = begin_expr;
auto __end = end_expr;
for (; __begin != __end; ++__begin) {
    range_declaration = *__begin;
    loop_statement
}
}*/

/*
A template parameter pack can only be TAD when it appears at the end of a parameter list, function templates can
have single parameters indicated, but in a class, if you indicate a parameter for a TA you must indicate the rest

*/