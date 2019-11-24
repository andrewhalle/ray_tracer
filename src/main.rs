use image;

// convert a floating point color channel to a RGB byte (with gamma correction)
fn color_f64_to_u8(x: f64, d: f64) -> u8 {
    let x1: f64 = (0.0 as f64).max(x * d);
    let x2: f64 = (1.0 as f64).min(x1);
    let x3: f64 = x2.powf(1.0 / 2.2);

    (x3 * 255.0) as u8
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Vector3 {
    x: f64,
    y: f64,
    z: f64,
}

impl Vector3 {
    fn new(x: f64, y: f64, z: f64) -> Vector3 {
        Vector3 { x, y, z }
    }

    fn add(v1: Vector3, v2: Vector3) -> Vector3 {
        Vector3 {
            x: v1.x + v2.x,
            y: v1.y + v2.y,
            z: v1.z + v2.z,
        }
    }

    fn subtract(v1: Vector3, v2: Vector3) -> Vector3 {
        Vector3 {
            x: v1.x - v2.x,
            y: v1.y - v2.y,
            z: v1.z - v2.z,
        }
    }

    fn scale(v: Vector3, factor: f64) -> Vector3 {
        Vector3 {
            x: v.x * factor,
            y: v.y * factor,
            z: v.z * factor,
        }
    }

    fn dot(v1: Vector3, v2: Vector3) -> f64 {
        v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
    }

    fn norm(self) -> Vector3 {
        Vector3::scale(self, 1.0 / Vector3::length(self))
    }

    fn cross(v1: Vector3, v2: Vector3) -> Vector3 {
        Vector3 {
            x: v1.y * v2.z - v1.z * v2.y,
            y: v1.z * v2.x - v1.x * v2.z,
            z: v1.x * v2.y - v1.y * v2.x,
        }
    }

    fn length(v: Vector3) -> f64 {
        Vector3::dot(v, v).sqrt()
    }
}

#[derive(Clone, Copy)]
struct Color {
    r: f64,
    g: f64,
    b: f64,
}

impl Color {
    fn new(r: f64, g: f64, b: f64) -> Color {
        Color { r, g, b }
    }

    fn black() -> Color {
        Color {
            r: 0.0,
            g: 0.0,
            b: 0.0,
        }
    }

    fn add(c1: Color, c2: Color) -> Color {
        Color {
            r: c1.r + c2.r,
            g: c1.g + c2.g,
            b: c1.b + c2.b,
        }
    }

    fn scale(c: Color, factor: f64) -> Color {
        Color {
            r: c.r * factor,
            g: c.g * factor,
            b: c.b * factor,
        }
    }

    fn times(c1: Color, c2: Color) -> Color {
        Color {
            r: c1.r * c2.r,
            g: c1.g * c2.g,
            b: c1.b * c2.b,
        }
    }
}

#[derive(Clone, Copy)]
struct Ray {
    from: Vector3,
    to: Vector3,
}

impl Ray {
    fn new(from: Vector3, to: Vector3) -> Ray {
        Ray { from, to }
    }

    fn direction(&self) -> Vector3 {
        Vector3::subtract(self.to, self.from).norm()
    }
}

struct Scene {
    objects: Vec<SceneObject>,
    lights: Vec<SceneLight>,
    camera: Option<SceneCamera>,
    d: f64,
    ambient_coef: f64,
}

impl Scene {
    fn new(ambient_coef: f64, d: f64) -> Scene {
        Scene {
            objects: Vec::new(),
            lights: Vec::new(),
            camera: None,
            d,
            ambient_coef,
        }
    }

    fn add_object(mut self, obj: SceneObject) -> Scene {
        self.objects.push(obj);

        self
    }

    fn add_light(mut self, light: SceneLight) -> Scene {
        self.lights.push(light);

        self
    }

    fn add_camera(
        mut self,
        center: Vector3,
        pointing_at: Vector3,
        up_direction: Vector3,
        f_distance: f64,
        img_width: f64,
        img_height: f64,
        num_pixels_x: u32,
        num_pixels_y: u32,
    ) -> Scene {
        let pointing_direction = Vector3::subtract(pointing_at, center).norm();
        let image_center = Vector3::add(center, Vector3::scale(pointing_direction, f_distance));
        let x_direction = Vector3::cross(pointing_direction, up_direction);
        let y_direction = Vector3::scale(up_direction, -1.0);
        let top_left = Vector3::add(
            image_center,
            Vector3::add(
                Vector3::scale(x_direction, img_width / -2.0),
                Vector3::scale(y_direction, img_height / -2.0),
            ),
        );
        self.camera = Some(SceneCamera {
            center,
            top_left,
            x_direction,
            y_direction,
            img_width,
            img_height,
            num_pixels_x,
            num_pixels_y,
        });

        self
    }

    fn trace_to(&self, path: &str) {
        if self.camera.is_none() {
            panic!("you need to add a camera before you can trace");
        }

        let mut imgbuf = image::ImageBuffer::new(
            self.camera.as_ref().unwrap().num_pixels_x,
            self.camera.as_ref().unwrap().num_pixels_y,
        );
        for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
            let Color { r, g, b } = self.trace_pixel(x, y);
            let r = color_f64_to_u8(r, self.d);
            let g = color_f64_to_u8(g, self.d);
            let b = color_f64_to_u8(b, self.d);

            *pixel = image::Rgb([r, g, b]);
        }

        imgbuf
            .save(path)
            .expect(&format!("could not save image: {}", path));
    }

    fn trace_pixel(&self, x: u32, y: u32) -> Color {
        let ray = Ray::new(
            self.camera.as_ref().unwrap().center,
            match self.camera.as_ref().unwrap().pixel_coordinates(x, y) {
                Some(p) => p,
                None => panic!("invalid pixel coordinates: {}, {}", x, y),
            },
        );

        self.trace_ray(ray)
    }

    fn trace_ray(&self, ray: Ray) -> Color {
        let intersection = self.shoot(ray);
        match intersection {
            Some((p, _, obj)) => {
                let distance = Vector3::length(Vector3::subtract(p, ray.from));
                Color::scale(
                    self.irradiance_at_point(ray, p, obj),
                    1.0 / distance.powi(2),
                )
            }
            None => Color {
                r: 0.0,
                g: 0.0,
                b: 0.0,
            },
        }
    }

    fn shoot(&self, ray: Ray) -> Option<(Vector3, f64, &SceneObject)> {
        let mut first_intersection: Option<(Vector3, f64, &SceneObject)> = None;

        for obj in self.objects.iter() {
            let hit_point = obj.intersect(ray);
            match hit_point {
                Some((p, t)) => {
                    if first_intersection.is_none()
                        || (t > 0.0 && t < first_intersection.unwrap().1)
                    {
                        first_intersection = Some((p, t, obj));
                    }
                }
                None => {}
            }
        }

        first_intersection
    }

    fn irradiance_at_point(&self, ray: Ray, point: Vector3, obj: &SceneObject) -> Color {
        match obj.material {
            SceneObjectMaterial::PureDiffuse { color } => {
                let mut direct_illumination = Color::black();

                for light in self.lights.iter() {
                    let shadow_ray = Ray::new(point, light.center);
                    match self.shoot(shadow_ray) {
                        Some((p, _, _)) => {
                            let hit_distance = Vector3::length(Vector3::subtract(p, point));
                            let light_distance =
                                Vector3::length(Vector3::subtract(light.center, point));

                            if (hit_distance < light_distance) {
                                direct_illumination = Color::add(
                                    direct_illumination,
                                    Color::scale(color, self.ambient_coef),
                                );
                            } else {
                                let mut shade =
                                    Vector3::dot(obj.normal_at(point), shadow_ray.direction());
                                if shade < 0.0 {
                                    shade = 0.0;
                                }

                                let distance =
                                    Vector3::length(Vector3::subtract(light.center, point));
                                shade /= distance.powi(2);

                                direct_illumination = Color::add(
                                    direct_illumination,
                                    Color::scale(
                                        color,
                                        self.ambient_coef
                                            + ((1.0 - self.ambient_coef) * shade * light.intensity),
                                    ),
                                );
                            }
                        }
                        None => {
                            let mut shade =
                                Vector3::dot(obj.normal_at(point), shadow_ray.direction());
                            if shade < 0.0 {
                                shade = 0.0;
                            }

                            let distance = Vector3::length(Vector3::subtract(light.center, point));
                            shade /= distance.powi(2);

                            direct_illumination = Color::add(
                                direct_illumination,
                                Color::scale(
                                    color,
                                    self.ambient_coef
                                        + ((1.0 - self.ambient_coef) * shade * light.intensity),
                                ),
                            );
                        }
                    }
                }

                direct_illumination
            }
            SceneObjectMaterial::PureSpecular { color } => {
                // compute reflection ray
                let reflected_direction = Vector3::subtract(
                    ray.direction(),
                    Vector3::scale(
                        obj.normal_at(point),
                        2.0 * Vector3::dot(ray.direction(), obj.normal_at(point)),
                    ),
                );

                Color::times(
                    color,
                    self.trace_ray(Ray::new(point, Vector3::add(point, reflected_direction))),
                )
            }
        }
    }
}

struct SceneObject {
    material: SceneObjectMaterial,
    kind: SceneObjectKind,
}

enum SceneObjectMaterial {
    PureDiffuse { color: Color },
    PureSpecular { color: Color },
}

enum SceneObjectKind {
    Plane(Vector3, Vector3),
    Sphere(Vector3, f64),
}

impl SceneObject {
    fn intersect(&self, ray: Ray) -> Option<(Vector3, f64)> {
        match self.kind {
            SceneObjectKind::Plane(center, normal) => {
                let denom = Vector3::dot(ray.direction(), normal);
                if denom > 0.0 {
                    return None;
                }

                let t = Vector3::dot(Vector3::subtract(center, ray.from), normal) / denom;

                if t > 0.0 {
                    Some((
                        Vector3::add(Vector3::scale(ray.direction(), t), ray.from),
                        t,
                    ))
                } else {
                    None
                }
            }
            SceneObjectKind::Sphere(center, radius) => {
                let eo = Vector3::subtract(ray.from, center);
                let v = Vector3::dot(eo, ray.direction());
                let disc = (v * v) - (Vector3::dot(eo, eo) - (radius * radius));
                if disc < 0.0 {
                    None
                } else {
                    let d = disc.sqrt();
                    let d_pos = d - v;
                    let d_neg = -(v + d);

                    if d_neg <= 0.0 && d_pos <= 0.0 {
                        None
                    } else if d_neg <= 0.0 {
                        let p = Vector3::add(ray.from, Vector3::scale(ray.direction(), d_pos));
                        if Vector3::dot(self.normal_at(p), ray.direction()) > 0.0 {
                            None
                        } else {
                            Some((p, d_pos))
                        }
                    } else {
                        let p = Vector3::add(ray.from, Vector3::scale(ray.direction(), d_neg));
                        if Vector3::dot(self.normal_at(p), ray.direction()) > 0.0 {
                            None
                        } else {
                            Some((p, d_neg))
                        }
                    }
                }
            }
        }
    }

    fn normal_at(&self, point: Vector3) -> Vector3 {
        match self.kind {
            SceneObjectKind::Plane(_center, normal) => normal,
            SceneObjectKind::Sphere(center, _radius) => Vector3::subtract(point, center).norm(),
        }
    }
}

struct Plane;
impl Plane {
    fn new(center: Vector3, normal: Vector3, material: SceneObjectMaterial) -> SceneObject {
        SceneObject {
            material,
            kind: SceneObjectKind::Plane(center, normal),
        }
    }
}

struct Sphere;
impl Sphere {
    fn new(center: Vector3, radius: f64, material: SceneObjectMaterial) -> SceneObject {
        SceneObject {
            material,
            kind: SceneObjectKind::Sphere(center, radius),
        }
    }
}

struct SceneLight {
    center: Vector3,
    intensity: f64,
}

impl SceneLight {
    fn new(center: Vector3, intensity: f64) -> SceneLight {
        SceneLight { center, intensity }
    }
}

#[derive(Debug)]
struct SceneCamera {
    center: Vector3,
    top_left: Vector3,
    x_direction: Vector3,
    y_direction: Vector3,
    img_width: f64,
    img_height: f64,
    num_pixels_x: u32,
    num_pixels_y: u32,
}

impl SceneCamera {
    fn pixel_coordinates(&self, x: u32, y: u32) -> Option<Vector3> {
        if x < self.num_pixels_x && y < self.num_pixels_y {
            return Some(Vector3::add(
                self.top_left,
                Vector3::add(
                    Vector3::scale(
                        self.x_direction,
                        ((x as f64) / (self.num_pixels_x as f64)) * self.img_width,
                    ),
                    Vector3::scale(
                        self.y_direction,
                        ((y as f64) / (self.num_pixels_y as f64)) * self.img_height,
                    ),
                ),
            ));
        }

        None
    }
}

fn main() {
    let scene = Scene::new(0.2, 10.0)
        .add_object(Sphere::new(
            Vector3::new(-1.1, 0.0, 0.0),
            1.0,
            SceneObjectMaterial::PureDiffuse {
                color: Color::new(1.0, 0.5, 0.5),
            },
        ))
        .add_object(Sphere::new(
            Vector3::new(1.1, 0.0, 0.0),
            1.0,
            SceneObjectMaterial::PureSpecular {
                color: Color::new(0.5, 0.1, 0.5),
            },
        ))
        .add_object(Plane::new(
            Vector3::new(0.0, -1.0, 0.0),
            Vector3::new(0.0, 1.0, 0.0).norm(),
            SceneObjectMaterial::PureDiffuse {
                color: Color::new(0.3, 0.3, 0.3),
            },
        ))
        .add_object(Plane::new(
            Vector3::new(0.0, 0.0, -5.0),
            Vector3::new(0.0, 0.0, 1.0).norm(),
            SceneObjectMaterial::PureDiffuse {
                color: Color::new(0.3, 0.3, 0.3),
            },
        ))
        .add_light(SceneLight::new(Vector3::new(10.0, 10.0, 0.0), 1000.0))
        .add_camera(
            Vector3::new(0.0, 0.0, 5.0),
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(0.0, 1.0, 0.0).norm(),
            2.0,
            2.0,
            1.5,
            1280,
            960,
        );

    scene.trace_to("out.png");
}

#[cfg(test)]
mod tests {
    use super::{Color, Ray, Sphere, Vector3};

    #[test]
    fn sphere_normal_at() {
        let sphere = Sphere::new(Vector3::new(0.0, 0.0, 0.0), 1.0, Color::new(0.0, 0.0, 0.0));
        assert_eq!(
            sphere.normal_at(Vector3::new(0.0, 0.0, 1.0)),
            Vector3::new(0.0, 0.0, 1.0)
        );
        assert_eq!(
            sphere.normal_at(Vector3::new(1.0, 0.0, 0.0)),
            Vector3::new(1.0, 0.0, 0.0)
        );

        let sphere = Sphere::new(Vector3::new(0.0, 0.0, 0.0), 2.3, Color::new(0.0, 0.0, 0.0));
        assert_eq!(
            sphere.normal_at(Vector3::new(0.0, 0.0, -2.3)),
            Vector3::new(0.0, 0.0, -1.0)
        );
    }

    #[test]
    fn sphere_intersect() {
        let sphere = Sphere::new(Vector3::new(0.0, 0.0, 0.0), 1.0, Color::new(0.0, 0.0, 0.0));

        let ray = Ray::new(Vector3::new(0.0, 0.0, 10.0), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(
            sphere.intersect(ray),
            Some((Vector3::new(0.0, 0.0, 1.0), 9.0))
        );
    }
}
