require 'scanf'

def with(*a)
  yield *a
end

class Object
  def as
    yield self
  end
end

class State
  def initialize(n, box)
    with(box.reduce(:+)/n){|stack_height| @offset = box.collect{|x| x-stack_height} }
    @last = @offset.each_index.reduce(0){|m,o| @offset[o] != 0 ? o : m }
  end
  
  def next_split(position, load)
    min_index = -1
    while position <= @last
      load += @offset[position]
      if load >=0 and min_index != -1
        break
      end
      if load <0 and min_index == -1
        min_index = position;
      end
      if position < @last
        position+=1
      else
        break
      end
    end
    [min_index, position, load]
  end
  
  def min_route(start, load)
    min_index, position, load = next_split(start,load)
    steps=position-start
    if position != @last
      with(position-min_index,@last-position) do |dfirst,dlast|
        min_route(position+1,load).as do |mrb|
          if dfirst+mrb < 2*dlast
            return steps+(2*dfirst)+1+mrb
          else
            return steps+(2*dlast)+dfirst
          end
        end
      end
    end
    if min_index != -1
      steps+=position-min_index
    end
    steps
  end
  
end

gets.to_i.times { puts State.new(gets.to_i,gets.strip.split.collect{|x| x.to_i}).min_route(0,0)*2 }
