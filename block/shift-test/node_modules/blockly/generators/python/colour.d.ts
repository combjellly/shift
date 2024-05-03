/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */
/**
 * @file Generating Python for colour blocks.
 */
import type { Block } from '../../core/block.js';
import type { PythonGenerator } from './python_generator.js';
import { Order } from './python_generator.js';
export declare function colour_picker(block: Block, generator: PythonGenerator): [string, Order];
export declare function colour_random(block: Block, generator: PythonGenerator): [string, Order];
export declare function colour_rgb(block: Block, generator: PythonGenerator): [string, Order];
export declare function colour_blend(block: Block, generator: PythonGenerator): [string, Order];
//# sourceMappingURL=colour.d.ts.map