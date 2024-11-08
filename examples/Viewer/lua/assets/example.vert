// This is similar to the default vertex shader (See https://love2d.org/wiki/love.graphics.newShader)
// However, these three matrices are used instead of the parameter `transformProjection`.
uniform mat4 projectionMatrix;
uniform mat4 viewMatrix;
uniform mat4 modelMatrix;

vec4 position(mat4 transformProjection, vec4 vertexPosition)
{
    return projectionMatrix * viewMatrix * modelMatrix * vertexPosition;
}