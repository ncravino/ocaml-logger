module type LevelLogger = sig 
	val err : string -> unit
	val info: string -> unit
	val debug : string -> unit
end

module type Logger = sig
	module Info : LevelLogger
	module Error: LevelLogger
	module Debug : LevelLogger
end

module Log = struct 

	module type LogFormatter = sig val formatter : string -> string end
	module type LogSink = sig val sink : out_channel end
	module Make(S : LogSink)(F : LogFormatter) : Logger = struct 
		module M = struct
			let printer t s = String.concat " " [t;s] |> F.formatter |> output_string S.sink
			let err = printer "Error:"
			let info = printer "Info:"
			let debug = printer "Debug:"
		end

		module Info : LevelLogger = struct
			include M
			let debug _ = ()
		end

		module Error : LevelLogger = struct
			include M
			let debug _ = ()
			let info _ = ()
		end

		module Debug : LevelLogger = struct
			include M
		end	

	end

	module Console = struct 
		module Custom = Make(struct let sink = stderr end)
		module Default = Custom(struct let formatter s = s end)
	end
	module File (F : sig val file : string end) = struct
		module Custom = Make(struct let sink = open_out F.file end)
		module Default = Custom(struct let formatter s = s end)
	end 

end


