#version 330

uniform vec2 resolution;
uniform float currentTime;
uniform vec3 camPos;
uniform vec3 camDir;
uniform vec3 camUp;
uniform sampler2D tex;
uniform bool showStepDepth;

in vec3 pos;

out vec3 color;

#define PI 3.1415926535897932384626433832795
#define RENDER_DEPTH 800
#define CLOSE_ENOUGH 0.00001

#define BACKGROUND -1
#define BALL 0
#define BASE 1

#define GREEN vec3(0.4, 1, 0.4)
#define LIGHT_BLUE vec3(0.4, 0.4, 1)
#define BLACK vec3(0, 0, 0)

#define GRADIENT(pt, func) vec3( \
    func(vec3(pt.x + 0.0001, pt.y, pt.z)) - func(vec3(pt.x - 0.0001, pt.y, pt.z)), \
    func(vec3(pt.x, pt.y + 0.0001, pt.z)) - func(vec3(pt.x, pt.y - 0.0001, pt.z)), \
    func(vec3(pt.x, pt.y, pt.z + 0.0001)) - func(vec3(pt.x, pt.y, pt.z - 0.0001)))

const vec3 LIGHT_POS[] = vec3[](vec3(5, 18, 10));

///////////////////////////////////////////////////////////////////////////////

vec3 getBackground(vec3 dir) {
  float u = 0.5 + atan(dir.z, -dir.x) / (2 * PI);
  float v = 0.5 - asin(dir.y) / PI;
  vec4 texColor = texture(tex, vec2(u, v));
  return texColor.rgb;
}

vec3 getRayDir() {
  vec3 xAxis = normalize(cross(camDir, camUp));
  return normalize(pos.x * (resolution.x / resolution.y) * xAxis + pos.y * camUp + 5 * camDir);
}

///////////////////////////////////////////////////////////////////////////////
float sphere(vec3 pt) {
  return length(pt) - 1;
}

//Non-linear cube
float cubeNonLinear(vec3 pt) {
  return max(max(abs(pt.x), abs(pt.y)), abs(pt.z)) - 1;
}

//Linear cube
float cubeLinear(vec3 pt) {
  vec3 q = abs(pt) - vec3(1);

  //Why sould we write length(max()) instead of max(length(), 0)
  return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
  }

float plane(vec3 pt) {
    return pt.y + 1.0;
}

///////////////////////////////////////////////////////////////////////////////

vec3 translation (vec3 pt, vec3 translationVector){
  return pt - translationVector;
}

//union is a reserved keyword so I changed function name
float unification (float sdf1, float sdf2){
  return min(sdf1, sdf2);
}

float difference (float sdf1, float sdf2){
  return max(sdf1, -sdf2);
}

float blend (float sdf1, float sdf2) {
    float k = 0.2; //'Smootheness' coefficient
    float h = clamp(0.5 + 0.5 * (sdf1 - sdf2) / k, 0, 1); //Set value to be in the range from 0 to 1
    return mix(sdf1, sdf2, h) - k * h * (1 - h); //Linearly interpolate values
}

float intersection (float sdf1, float sdf2){
  return max(sdf1, sdf2);
}

float sdfWithoutPlane(vec3 pt) {
    vec3 displacementVector = vec3(1, 0, 1);
    return unification
    (
        unification
        (
            unification
            (
                cubeLinear(translation(pt, vec3(-3, 0, -3))),
                sphere(translation(pt, vec3(-3, 0, -3) + displacementVector))
            ),
            difference
            (
                cubeLinear(translation(pt, vec3(3, 0, -3))),
                sphere(translation(pt, vec3(3, 0, -3) + displacementVector))
            )
        ),
        unification
        (
            blend
            (
                cubeLinear(translation(pt, vec3(-3, 0, 3))),
                sphere(translation(pt, vec3(-3, 0, 3) + displacementVector))
            ),
            intersection
            (
                cubeLinear(translation(pt, vec3(3, 0, 3))),
                sphere(translation(pt, vec3(3, 0, 3) + displacementVector))
            )
        )
    );
}

float sdfWithPlane(vec3 pt) {
    return unification (plane(pt), sdfWithoutPlane(pt));
}
///////////////////////////////////////////////////////////////////////////////

vec3 getNormal(vec3 pt) {
  return normalize(GRADIENT(pt, sdfWithPlane));
}

vec3 getColor(vec3 pt) {
    float distToPlane = plane(pt);
    float distToNonPlaneObjects = sdfWithoutPlane(pt);

    if(abs(distToPlane) < CLOSE_ENOUGH ){
        //We are colouring a plane
        float x = mod(distToNonPlaneObjects, 5);
        if(x >= 4.75) return BLACK;
        else return mix(GREEN, LIGHT_BLUE, mod(x,1));
    }

    //We are colouring non plane objects
    return vec3(1);
}

///////////////////////////////////////////////////////////////////////////////

float shade(vec3 eye, vec3 pt, vec3 n) {
  float val = 0;
  
  val += 0.1;  // Ambient
  
  for (int i = 0; i < LIGHT_POS.length(); i++) {
    vec3 l = normalize(LIGHT_POS[i] - pt); 
    val += max(dot(n, l), 0);
  }
  return val;
}

vec3 illuminate(vec3 camPos, vec3 rayDir, vec3 pt) {
  vec3 c, n;
  n = getNormal(pt);
  c = getColor(pt);
  return shade(camPos, pt, n) * c;
}

///////////////////////////////////////////////////////////////////////////////

vec3 raymarch(vec3 camPos, vec3 rayDir) {
  int step = 0;
  float t = 0;

  for (float d = 1000; step < RENDER_DEPTH  && abs(d) > CLOSE_ENOUGH ; t += abs(d) )
  {
    vec3 currentPosition = camPos + t * rayDir;
    d = sdfWithPlane(currentPosition);
    step++;
  }

  if (step == RENDER_DEPTH) {
    return getBackground(rayDir);
  } else if (showStepDepth) {
    return vec3(float(step) / RENDER_DEPTH);
  } else {
    return illuminate(camPos, rayDir, camPos + t * rayDir);
  }
}

///////////////////////////////////////////////////////////////////////////////

void main() {
  color = raymarch(camPos, getRayDir());
}