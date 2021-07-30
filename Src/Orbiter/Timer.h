#pragma once
#include <Windows.h>

namespace TChapman500
{
	class Timer
	{
	public:
		Timer()
		{
			LARGE_INTEGER frequency;
			QueryPerformanceFrequency(&frequency);
			Frequency = (double)frequency.QuadPart;
			Difference.QuadPart = 0ULL;
			QueryPerformanceCounter(&Start);
			Current.QuadPart = Start.QuadPart;
		}
		
		~Timer() {}
		
		double Lap()
		{
			QueryPerformanceCounter(&Current);
			Difference.QuadPart = Current.QuadPart - Start.QuadPart;
			Start.QuadPart = Current.QuadPart;
			return (double)Difference.QuadPart / Frequency;
		}
		
		double RunTime()
		{
			QueryPerformanceCounter(&Current);
			Difference.QuadPart = Current.QuadPart - Start.QuadPart;
			return (double)Difference.QuadPart / Frequency;
		}

	private:

		double Frequency = 0.0;
		LARGE_INTEGER Start;
		LARGE_INTEGER Current;
		LARGE_INTEGER Difference;
	};
}
