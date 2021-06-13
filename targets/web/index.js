var exportObject = {
    internal: {
        memory: null,
        malloc: null,
        free: null,
        call: null,
    },
    allocateString: (s) => {
        if (exportObject.internal.memory == null) {
            return null;
        }

        var encoded = exportObject.utf8encoder.encode(s);

        var offset = exportObject.internal.malloc(encoded.length + 1);

        exportObject.internal.memory.set(encoded, offset)
        exportObject.internal.memory[offset + encoded.length] = 0;

        return {
            start: offset,
            size: encoded.length + 1
        };
    },
    freeString: (str) => {
        if (exportObject.internal.memory == null) {
            return;
        }

        if (str == null) {
            return null;
        }

        exportObject.internal.free(str.start);
    },
    utf8decoder: new TextDecoder("utf-8"),
    utf8encoder: new TextEncoder(),
    getString: (offset) => {
        if (exportObject.internal.memory == null) {
            return "";
        }

        if (offset == 0) {
            return null;
        }

        let offset2 = offset;
        while (exportObject.internal.memory[offset2] != 0) {
            offset2++;
        }
        return exportObject.utf8decoder.decode(exportObject.internal.memory.subarray(offset, offset2));
    },
    getCallbackBuffer: () => {
        var addr = exportObject.internal.get_callback_buffer();
        return new Int32Array(exportObject.internal.buffer, addr, 10);
    }
};

var wasm_callbacks = {}
// reserve indexes -1,0,1,2,3 for use as constants
var wasm_object = [null, true, false, window]
var wasm_object_freelist = []
var wasm_fds = []
var wasm_fd_freelist = []

function save_wasm_object(v) {
    if (v === null) { return 0; }
    if (v === undefined) { return -1; }
    if (v === true) { return 1; }
    if (v === false) { return 2; }
    if (wasm_object_freelist.length === 0) {
        return wasm_object.push(v) - 1;
    }
    else {
        var i = wasm_object_freelist.pop();
        wasm_object[i] = v;
        return i;
    }
}
function free_wasm_object(_id) {
    if (_id > 3) {
        wasm_object[_id] = null;
        wasm_object_freelist.push(_id);
    }
}

var importObject = {
    env: {
        abort_js: (_err) => {
            var msg = exportObject.getString(_err);
            debugger;
            throw msg;
        },
        abort: () => {
            debugger;
            throw "terminated with abort()";
        },
        fopen_js: (offset, n) => {
            if (wasm_fd_freelist.length === 0 && wasm_fds.length === 64) return 0;
            var filename = exportObject.utf8decoder.decode(exportObject.internal.memory.subarray(offset, offset + n));
            var f = wasm_get_file(filename);
            if (!f) return 0;

            if (wasm_fd_freelist.length === 0) {
                return wasm_fds.push(f);
            } else {
                var i = wasm_fd_freelist.pop();
                wasm_fds[i - 1] = f;
                return i;
            }
        },
        fclose_js: (fd) => {
            if (wasm_fds[fd - 1] === undefined) return 1;
            wasm_fds[fd - 1] = undefined;
            wasm_fd_freelist.push(fd);
            return 0;
        },
        fwrite_js: (offset, n, fd) => {
            var txt = exportObject.utf8decoder.decode(exportObject.internal.memory.subarray(offset, offset + n));
            if (fd == 2) {
                document.getElementById("stdout").value += txt;
            }
            else if (fd == 3) {
                document.getElementById("stderr").value += txt;
            }
        },
        fread_js: (offset, sz, n, fd) => {
            var f = wasm_fds[fd - 1];
            if (f === undefined) return 0;
            var m = Math.floor(f.length / sz);
            if (m > n) { m = n; }
            if (m == 0) return 0;
            var to_copy = f.subarray(0, m * sz);
            wasm_fds[fd - 1] = f.subarray(m * sz);
            exportObject.internal.memory.set(to_copy, offset);
            return m;
        },
        log: (_str) => {
            console.log(exportObject.getString(_str));
        },
        memory_set: (_id, offset) => {
            exportObject.internal.memory.set(wasm_object[_id], offset);
        },
        object_from_i32: (i) => {
            return save_wasm_object(i);
        },
        object_to_i32: (i) => {
            return wasm_object[i];
        },
        object_set_property_o: (_id, _id2, _id3) => {
            wasm_object[_id][wasm_object[_id2]] = wasm_object[_id3];
        },
        object_get_property_o: (_id, _id2) => {
            return save_wasm_object(wasm_object[_id][wasm_object[_id2]]);
        },
        object_get_integer_property: (_id, _prop) => {
            return wasm_object[_id][exportObject.getString(_prop)];
        },
        object_from_string: (_text) => {
            return save_wasm_object(exportObject.getString(_text));
        },
        object_allocate_string: (_id) => {
            return exportObject.allocateString(wasm_object[_id]).start;
        },
        object_json_parse: (_id) => {
            try {
                return save_wasm_object(JSON.parse(wasm_object[_id]));
            } catch (e) {
                return -1;
            }
        },
        object_json_stringify: (_id) => {
            try {
                return save_wasm_object(JSON.stringify(wasm_object[_id]));
            } catch (e) {
                return -1;
            }
        },
        object_get_window: (_id) => {
            return 3;
        },
        object_btoa: (_id) => {
            try {
                return save_wasm_object(btoa(wasm_object[_id]));
            } catch (e) {
                return -1;
            }
        },
        object_atob: (_id) => {
            try {
                return save_wasm_object(atob(wasm_object[_id]));
            } catch (e) {
                return -1;
            }
        },
        new_rtcpeerconnection: (_id) => {
            return save_wasm_object(new RTCPeerConnection(wasm_object[_id]));
        },
        new_urlsearchparams: (_id) => {
            return save_wasm_object(new URLSearchParams(wasm_object[_id]));
        },
        register_callback_token: (i, _func, _ctx) => {
            wasm_callbacks[i] = [_func, _ctx]
        },
        convert_callback_to_object: (_cb) => {
            return save_wasm_object((event) => {
                var event_buffer = exportObject.getCallbackBuffer();
                var x = save_wasm_object(event);
                event_buffer[0] = x;
                callwasm(_cb);
                free_wasm_object(x);
            });
        },
        deregister_callback_token: (i) => {
            delete wasm_callbacks[i];
        },
        object_log: (_id) => {
            console.log(wasm_object[_id]);
        },
        object_equals: (_id1, _id2) => {
            return wasm_object[_id1] === wasm_object[_id2];
        },
        object_copy: (_id) => {
            return save_wasm_object(wasm_object[_id]);
        },
        jscall_call_o: (_id, _a1) => {
            return save_wasm_object(wasm_object[_id].call(wasm_object[_a1]));
        },
        jscall_call_oo: (_id, _a1, _a2) => {
            return save_wasm_object(wasm_object[_id].call(wasm_object[_a1], wasm_object[_a2]));
        },
        jscall_call_ooo: (_id, _a1, _a2, _a3) => {
            return save_wasm_object(wasm_object[_id].call(wasm_object[_a1], wasm_object[_a2], wasm_object[_a3]));
        },
        jscall_call_oooo: (_id, _a1, _a2, _a3, _a4) => {
            return save_wasm_object(wasm_object[_id].call(wasm_object[_a1], wasm_object[_a2], wasm_object[_a3], wasm_object[_a4]));
        },
        jscall_object: (_id) => {
            return save_wasm_object(wasm_object[_id]());
        },
        jscall_object_o: (_id, _a1) => {
            return save_wasm_object(wasm_object[_id](wasm_object[_a1]));
        },
        jscall_object_oo: (_id, _a1, _a2) => {
            return save_wasm_object(wasm_object[_id](wasm_object[_a1], wasm_object[_a2]));
        },
        jscall_object_ooo: (_id, _a1, _a2, _a3) => {
            return save_wasm_object(wasm_object[_id](wasm_object[_a1], wasm_object[_a2], wasm_object[_a3]));
        },
        jscall_object_oooo: (_id, _a1, _a2, _a3, _a4) => {
            return save_wasm_object(wasm_object[_id](wasm_object[_a1], wasm_object[_a2], wasm_object[_a3], wasm_object[_a4]));
        },
        jscall_object_i32: (_id, i) => {
            return save_wasm_object(wasm_object[_id](i));
        },
        jscall_object_f32: (_id, i) => {
            return save_wasm_object(wasm_object[_id](i));
        },
        new_array: () => {
            return save_wasm_object([]);
        },
        free_object: free_wasm_object,
        object_get_context: (_id, _text) => { return save_wasm_object(wasm_object[_id].getContext(exportObject.getString(_text))); },
        object_parent_node: (_id) => { return save_wasm_object(wasm_object[_id].parentNode); },
        canvas_get_extents: (_id, _addr) => {
            var i = exportObject.internal.memory32;
            var c = wasm_object[_id];
            i[_addr >> 2] = c.width;
            i[(_addr >> 2) + 1] = c.height;
        },
        canvas_set_extents: (_id, width, height) => { wasm_object[_id].width = width; wasm_object[_id].height = height; },
        window_device_pixel_ratio: () => { return window.devicePixelRatio; },
        context_scale: (_id, x, y) => { wasm_object[_id].scale(x, y); },
        context_fill_style: (_id, _text) => { wasm_object[_id].fillStyle = exportObject.getString(_text); },
        context_stroke_style: (_id, _text) => { wasm_object[_id].strokeStyle = exportObject.getString(_text); },
        context_line_width: (_id, width) => { wasm_object[_id].lineWidth = width; },
        context_font: (_id, _text) => { wasm_object[_id].font = exportObject.getString(_text); },
        context_stroke_rect: (_id, x, y, w, h) => { wasm_object[_id].strokeRect(x, y, w, h); },
        context_fill_rect: (_id, x, y, w, h) => { wasm_object[_id].fillRect(x, y, w, h); },
        context_fill_text: (_id, _text, x, y) => { wasm_object[_id].fillText(exportObject.getString(_text), x, y); },
        context_begin_path: (_id) => { wasm_object[_id].beginPath(); },
        context_line_to: (_id, x, y) => { wasm_object[_id].lineTo(x, y); },
        context_move_to: (_id, x, y) => { wasm_object[_id].moveTo(x, y); },
        context_stroke: (_id) => { wasm_object[_id].stroke(); },
        on_frame: (_cb) => { var f = (event) => { callwasm(_cb); requestAnimationFrame(f); }; f(); },
        add_mouse_touch_event_listeners: (_id, _cbmouse, _cbtouch) => {
            (function () {
                var canvas = wasm_object[_id];

                var getMouseCallback = (code) => {
                    return (event) => {
                        var rect = canvas.getBoundingClientRect();

                        var event_buffer = exportObject.getCallbackBuffer();
                        event_buffer[0] = code;
                        event_buffer[1] = event.clientX - rect.x;
                        event_buffer[2] = event.clientY - rect.y;
                        callwasm(_cbmouse);

                        event.preventDefault();
                    };
                };
                canvas.addEventListener('mouseenter', getMouseCallback(0));
                canvas.addEventListener('mouseleave', getMouseCallback(1));
                canvas.addEventListener('mousemove', getMouseCallback(2));
                canvas.addEventListener('mousedown', getMouseCallback(3));
                canvas.addEventListener('mouseup', getMouseCallback(4));

                var getTouchCallback = (code) => {
                    return (event) => {
                        var rect = canvas.getBoundingClientRect();
                        var touches = event.changedTouches;

                        var event_buffer = exportObject.getCallbackBuffer();
                        for (var i = 0; i < touches.length; i++) {
                            var touch = touches[i];
                            event_buffer[0] = code;
                            event_buffer[1] = touch.identifier
                            event_buffer[2] = touch.pageX - rect.x;
                            event_buffer[3] = touch.pageY - rect.y;
                            callwasm(_cbtouch);
                        }

                        event.preventDefault();
                    };
                };
                canvas.addEventListener('touchstart', getTouchCallback(1));
                canvas.addEventListener('touchend', getTouchCallback(2));
                canvas.addEventListener('touchmove', getTouchCallback(0));
                canvas.addEventListener('touchcancel', getTouchCallback(2));
            }())
        },
        glclearColor: (_id, r, g, b, a) => { wasm_object[_id].clearColor(r, g, b, a); },
        glclearDepth: (_id, d) => { wasm_object[_id].clearDepth(d); },
        glclearBuffer: (_id) => {
            var gl = wasm_object[_id];
            gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        },
        glcreateVertexShader: (_id) => { return save_wasm_object(wasm_object[_id].createShader(wasm_object[_id].VERTEX_SHADER)); },
        glcreateFragmentShader: (_id) => { return save_wasm_object(wasm_object[_id].createShader(wasm_object[_id].FRAGMENT_SHADER)); },
        glshaderSourceCompile: (_id, _shader, _text) => {
            var text = exportObject.getString(_text);
            var gl = wasm_object[_id];
            var shader = wasm_object[_shader];
            gl.shaderSource(shader, text);
            gl.compileShader(shader);
            if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
                alert('An error occurred compiling the shaders: ' + gl.getShaderInfoLog(shader));
                debugger;
            }
        },
        glcreateProgram: (_id) => { return save_wasm_object(wasm_object[_id].createProgram()); },
        glattachShader: (_id, _program, _shader) => { wasm_object[_id].attachShader(wasm_object[_program], wasm_object[_shader]); },
        gldeleteShader: (_id, _shader) => { wasm_object[_id].deleteShader(wasm_object[_shader]); },
        gllinkProgram: (_id, _program) => {
            var shaderProgram = wasm_object[_program];
            var gl = wasm_object[_id];
            gl.linkProgram(shaderProgram);
            if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
                alert('Unable to initialize the shader program: ' + gl.getProgramInfoLog(shaderProgram));
                debugger;
            }
        },
        glgetAttribLocation: (_id, _program, _text) => {
            return save_wasm_object(wasm_object[_id].getAttribLocation(wasm_object[_program], exportObject.getString(_text)));
        },
        glgetUniformLocation: (_id, _program, _text) => {
            return save_wasm_object(wasm_object[_id].getUniformLocation(wasm_object[_program], exportObject.getString(_text)));
        },
        glcreateBuffer: (_id) => { return save_wasm_object(wasm_object[_id].createBuffer()); },
        glbindArrayBuffer: (_id, _buffer) => {
            var gl = wasm_object[_id];
            gl.bindBuffer(gl.ARRAY_BUFFER, wasm_object[_buffer]);
        },
        glbufferDataArray: (_id, _begin, _end) => {
            var gl = wasm_object[_id];
            gl.bufferData(gl.ARRAY_BUFFER, exportObject.internal.buffer.slice(_begin, _end), gl.STATIC_DRAW);
        },
        glvertexAttribPointer: (_id, _attr, n) => {
            var gl = wasm_object[_id];
            gl.vertexAttribPointer(wasm_object[_attr], n, gl.FLOAT, false, 0, 0);
        },
        glenableVertexAttribArray: (_id, _attr) => { wasm_object[_id].enableVertexAttribArray(wasm_object[_attr]); },
        gluseProgram: (_id, _program) => {
            wasm_object[_id].useProgram(wasm_object[_program]);
        },
        gluniformMatrix4fv: (_id, _attr, _begin) => {
            wasm_object[_id].uniformMatrix4fv(wasm_object[_attr], false, new Float32Array(exportObject.internal.buffer, _begin, 16));
        },
        gldrawTriangleStrip: (_id, offset, vertexCount) => {
            var gl = wasm_object[_id];
            gl.drawArrays(gl.TRIANGLE_STRIP, offset, vertexCount);
        },
        glenable: (_id, cap) => { wasm_object[_id].enable(cap); },
        gldepthFunc: (_id, cap) => { wasm_object[_id].depthFunc(cap); },
        tan: (x) => Math.tan(x),
        cos: (x) => Math.cos(x),
        sin: (x) => Math.sin(x),
        random: () => Math.random(),
    }
};

function callwasm(id) {
    if (id in wasm_callbacks) {
        var cb = wasm_callbacks[id];
        exportObject.internal.call(cb[0], cb[1]);
    }
}

function on_finish_loading(native) {
    console.log("loaded");
    document.getElementById("compile-button").onclick = function (e) {
        wasm_fd_freelist = [];
        wasm_fds = [new Uint8Array(), new Uint8Array(), new Uint8Array()];
        document.getElementById("stdout").value = "";
        document.getElementById("stderr").value = "";
        native.wasmmain(document.getElementById("debugmode").checked);
    };
}

function wasm_get_file(file) {
    if (file == "mindustry.h") {
        return exportObject.utf8encoder.encode(prelude_session.getValue());
    } else if (file == "main.c") {
        return exportObject.utf8encoder.encode(main_session.getValue());
    } else {
        return null;
    }
}

fetch('mindustry.h').then(response => {
    if (!response.ok) {
        throw new Error('Network response was not ok');
    }
    return response.blob();
}).then((blob) => {
    return blob.text();
}).then((blob) => {
    prelude_session.setValue(blob);
});

fetch('extensions.html').then(response => {
    if (!response.ok) {
        throw new Error('Network response was not ok');
    }
    return response.blob();
}).then((blob) => {
    return blob.text();
}).then((blob) => {
    document.getElementById("help").innerHTML = blob;
});

if (typeof WebAssembly === "object") {
    if (typeof WebAssembly.instantiateStreaming !== "function") {
        WebAssembly.instantiateStreaming = (p, importObject) =>
            p.then(response =>
                response.arrayBuffer()
            ).then(bytes =>
                WebAssembly.instantiate(bytes, importObject)
            );
    }
    var memory = new WebAssembly.Memory({ initial: 100, maximum: 1000 });
    importObject.env.memory = memory;
    WebAssembly.instantiateStreaming(fetch('mindcc-wasmlib.wasm'), importObject).then(results => {
        var native = results.instance.exports;
        var buffer = memory.buffer;
        exportObject.internal.malloc = native.malloc;
        exportObject.internal.buffer = buffer;
        exportObject.internal.memory = new Int8Array(buffer);
        exportObject.internal.memory32 = new Int32Array(buffer);
        exportObject.internal.memoryf32 = new Float32Array(buffer);
        exportObject.internal.free = native.free;
        exportObject.internal.call = native.call;
        exportObject.internal.callback = native.callback;
        exportObject.internal.get_callback_buffer = native.get_callback_buffer;
        on_finish_loading(native);
    });
} else {
    var script = document.createElement('script');
    script.src = 'mindcc-wasm.js';
    script.type = 'text/javascript';

    script.onload = function () {
        var buffer = new ArrayBuffer(1024 * 1024 * 10);
        var native = instantiate(importObject.env, { 'buffer': buffer });
        exportObject.internal.malloc = native.malloc;
        exportObject.internal.buffer = buffer;
        exportObject.internal.memory = new Int8Array(buffer);
        exportObject.internal.memory32 = new Int32Array(buffer);
        exportObject.internal.memoryf32 = new Float32Array(buffer);
        exportObject.internal.free = native.free;
        exportObject.internal.call = native.call;
        exportObject.internal.callback = native.callback;
        exportObject.internal.get_callback_buffer = native.get_callback_buffer;
        on_finish_loading(native);
    };

    document.getElementsByTagName('head')[0].appendChild(script);
}
