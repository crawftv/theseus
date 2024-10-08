const std = @import("std");
const zap = @import("zap");
const sqlite = @import("sqlite");
const fs = std.fs;

fn queryBookChapter(book_name: []const u8, chapter: u32) ![]const u8 {

    var db =  try sqlite.Db.init(.{
        .mode = sqlite.Db.Mode{ .File = "path/to/your/database.db" },
        .open_flags = .{},
        .threading_mode = .MultiThread,
    });
    // Prepare the SQL statement
    const query =
    \\ Select json_group_array(json_object('line', line, 'lineNumber', lineNumber )) as json_data
    \\ FROM fullLines
    \\ INNER JOIN books ON fullLines.bookId = books.id
    \\ WHERE books.title = ? AND fullLines.chapter = ?
    \\ ORDER BY lineNumber ASC"
;
    var stmt = try db.prepare(query);
    defer stmt.deinit();

    // Bind the parameters to the statement

    // Execute the query and process the results
    const row = try stmt.one(
        struct {
            json_data: []const u8,
            },.{},
        .{
            .title = book_name,
            .chapter = chapter
        },
    );
    return row.json_data;
}

fn on_request(r: zap.Request) void {
    if (r.path) |the_path| {
        if (std.mem.eql(u8, the_path, "/")) {
            // Serve index.html for the root route
            const file_path = "public/index.html";
            serveFile(r, file_path) catch |err| {
                std.debug.print("Error serving file: {s}\n", .{@errorName(err)});
                r.setStatus(.not_found);

            };
        } else if (std.mem.startsWith(u8, the_path, "/books/")) {
            // Handle the "/books/{text}/{id}" route
            var parts = std.mem.split(u8, the_path[7..], "/");
            const book_name = parts.next() orelse {
                r.setStatus(.not_found);
                return;
            };
            const chapter_id = std.fmt.parseInt(u32, parts.next() orelse "", 10) catch {
                r.setStatus(.not_found);
                return;
            };

            // // Process the extracted book_name and book_id
            // std.debug.print("Book Name: {s}, Book ID: {}\n", .{book_name, book_id});

            // Send a response
            const dbResult = try queryBookChapter(book_name,chapter_id);
            r.sendBody(dbResult) catch return;
        } else {
            // Handle other routes or requests
            r.setStatus(.not_found);
        }
    } else {
        r.setStatus(.not_found);
    }
}

fn serveFile(r: zap.Request, file_path: []const u8) !void {
    const file = try fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_contents = try file.readToEndAlloc(std.heap.page_allocator, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    r.setHeader("Content-Type", "text/html") catch return;
    r.sendBody(file_contents) catch return;
}

pub fn main() !void {
    var listener = zap.HttpListener.init(.{
        .port = 3000,
        .on_request = on_request,
        .log = true,
        .max_clients = 100000,
    });
    try listener.listen();

    std.debug.print("Listening on 0.0.0.0:3000\n", .{});

    // start worker threads
    zap.start(.{
        .threads = 2,
        .workers = 2,
    });
}