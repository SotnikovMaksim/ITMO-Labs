#include <cstddef>

template< typename T >
bool comp(T left, T right, bool descending)
{
	if (descending)
	{
		return left < right;
	}
	return right < left;
}

template< typename T >
size_t partition(size_t left_border, size_t right_border, T* data, bool descending)
{
	size_t left = left_border;
	size_t right = right_border;
	T pivot = data[(left + right + 1) / 2];
	while (left <= right)
	{
		while (comp< T >(pivot, data[left], descending))
		{
			left++;
		}
		while (comp< T >(data[right], pivot, descending))
		{
			right--;
		}
		if (left <= right)
		{
			std::swap(data[left++], data[right--]);
		}
	}
	return right;
}
template< typename T, bool descending >
void quicksort(size_t left_border, size_t right_border, T* data)
{
	while (left_border < right_border)
	{
		size_t pivotInd = partition(left_border, right_border, data, descending);
		if (pivotInd - left_border <= right_border - pivotInd)
		{
			quicksort< T, descending >(left_border, pivotInd, data);
			left_border = pivotInd + 1;
		}
		else
		{
			quicksort< T, descending >(pivotInd + 1, right_border, data);
			right_border = pivotInd;
		}
	}
}