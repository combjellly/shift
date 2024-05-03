/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */
/**
 * @file Generating JavaScript for colour blocks.
 */
import type { Block } from '../../core/block.js';
import type { JavascriptGenerator } from './javascript_generator.js';
import { Order } from './javascript_generator.js';
export declare function colour_picker(block: Block, generator: JavascriptGenerator): [string, Order];
export declare function colour_random(block: Block, generator: JavascriptGenerator): [string, Order];
export declare function colour_rgb(block: Block, generator: JavascriptGenerator): [string, Order];
export declare function colour_blend(block: Block, generator: JavascriptGenerator): [string, Order];
//# sourceMappingURL=colour.d.ts.map