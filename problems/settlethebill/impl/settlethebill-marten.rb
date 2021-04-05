def zero_subsets(s, debts)
	return s == 0 ? 1 : 0 if debts.empty?
	d = debts[1..-1]
	return zero_subsets(s + debts[0], d) +
		   zero_subsets(s,            d)
end

def run
	# read stuff
	npeople, ndebts = gets.split.map(&:to_i)
	debts = [0]*npeople
	ndebts.times do
		from, to, amount = gets.split.map(&:to_i)
		debts[from] -= amount
		debts[to]   += amount
	end

	puts "# #{Time.now.inspect} DEBTS: #{debts.inspect}"

	# compute number of subsets
	zs = zero_subsets(0, debts)

	# output!
	if zs == 2
		puts "tight"
	else
		puts "loose"
	end
end

gets.to_i.times { run }
