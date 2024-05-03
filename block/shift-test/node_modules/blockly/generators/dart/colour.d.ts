/**
 * @license
 * Copyright 2014 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */
/**
 * @file Generating Dart for colour blocks.
 */
import type { Block } from '../../core/block.js';
import type { DartGenerator } from './dart_generator.js';
import { Order } from './dart_generator.js';
export declare function colour_picker(block: Block, generator: DartGenerator): [string, Order];
export declare function colour_random(block: Block, generator: DartGenerator): [string, Order];
export declare function colour_rgb(block: Block, generator: DartGenerator): [string, Order];
export declare function colour_blend(block: Block, generator: DartGenerator): [string, Order];
//# sourceMappingURL=colour.d.ts.map