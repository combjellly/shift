import * as Blockly from 'blockly';

export const shiftGenerator = new Blockly.Generator('SHIFT');

const Order = {
  ATOMIC: 0,
};

shiftGenerator.forBlock['play'] = function(block, generator) {
  const sampleName = generator.valueToCode(
      block, 'PLAY', Order.ATOMIC);
  const code = `play.${sampleName}`;
  return code;
};

shiftGenerator.forBlock['num'] = function(block) {
  const num = block.getFieldValue('NUM');
  const code = `{num}`;
  return [code, Order.ATOMIC];
};

shiftGenerator.forBlock['member'] = function(block, generator) {
  const name = block.getFieldValue('MEMBER_NAME');
  const value = generator.valueToCode(
      block, 'MEMBER_VALUE', Order.ATOMIC);
  const code = `"${name}": ${value}`;
  return code;
};


