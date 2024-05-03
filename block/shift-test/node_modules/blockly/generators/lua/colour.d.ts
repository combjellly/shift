/**
 * @license
 * Copyright 2016 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */
/**
 * @file Generating Lua for colour blocks.
 */
import type { Block } from '../../core/block.js';
import type { LuaGenerator } from './lua_generator.js';
import { Order } from './lua_generator.js';
export declare function colour_picker(block: Block, generator: LuaGenerator): [string, Order];
export declare function colour_random(block: Block, generator: LuaGenerator): [string, Order];
export declare function colour_rgb(block: Block, generator: LuaGenerator): [string, Order];
export declare function colour_blend(block: Block, generator: LuaGenerator): [string, Order];
//# sourceMappingURL=colour.d.ts.map