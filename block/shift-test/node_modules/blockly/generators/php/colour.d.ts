/**
 * @license
 * Copyright 2015 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */
/**
 * @file Generating PHP for colour blocks.
 */
import type { Block } from '../../core/block.js';
import { Order } from './php_generator.js';
import type { PhpGenerator } from './php_generator.js';
export declare function colour_picker(block: Block, generator: PhpGenerator): [string, Order];
export declare function colour_random(block: Block, generator: PhpGenerator): [string, Order];
export declare function colour_rgb(block: Block, generator: PhpGenerator): [string, Order];
export declare function colour_blend(block: Block, generator: PhpGenerator): [string, Order];
//# sourceMappingURL=colour.d.ts.map