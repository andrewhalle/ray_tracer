use image;

#[derive(Clone, Copy, Debug)]
struct Vector3 {
    x: f64,
    y: f64,
    z: f64,
}

impl Vector3 {
    fn new(x: f64, y: f64, z: f64) -> Vector3 {
        Vector3 { x, y, z }
    }

    fn norm(&self) -> Vector3 {
        let norm = (self.x * self.x + self.y * self.y + self.z * self.z).sqrt();
        Vector3 {
            x: self.x / norm,
            y: self.y / norm,
            z: self.z / norm,
        }
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
}

impl Scene {
    fn new() -> Scene {
        Scene {
            objects: Vec::new(),
            lights: Vec::new(),
            camera: None,
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

        println!("{:?}", self.camera.as_ref().unwrap());

        self
    }

    fn trace_to(&self, path: &str) {
        if self.camera.is_none() {
            panic!("you need to add a camera before you can trace");
        }

        let mut imgbuf = image::ImageBuffer::new(640, 480);
        for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
            let Color { r, g, b } = self.trace_pixel(x, y);
            let r = (r * 255.0) as u8;
            let g = (g * 255.0) as u8;
            let b = (b * 255.0) as u8;

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
            Some((p, _, obj)) => self.irradiance_at_point(p, obj),
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
                    if first_intersection.is_none() || t < first_intersection.unwrap().1 {
                        first_intersection = Some((p, t, obj));
                    }
                }
                None => {}
            }
        }

        first_intersection
    }

    fn irradiance_at_point(&self, point: Vector3, obj: &SceneObject) -> Color {
        let mut irradiance = Color::black();

        for light in self.lights.iter() {
            let shadow_ray = Ray::new(point, light.center);
            match self.shoot(shadow_ray) {
                Some(_) => {}
                None => {
                    let mut shade = Vector3::dot(obj.normal_at(point), shadow_ray.direction());
                    if shade < 0.0 {
                        shade = 0.0;
                    }

                    irradiance =
                        Color::add(irradiance, Color::scale(obj.color, 0.2 + (0.8 * shade)));
                }
            }
        }

        irradiance
    }
}

struct SceneObject {
    color: Color,
    kind: SceneObjectKind,
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
                if denom.abs() < 1e-9 {
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
                    let p = Vector3::add(center, Vector3::scale(ray.direction(), v - d));
                    Some((p, Vector3::length(Vector3::subtract(p, center))))
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
    fn new(center: Vector3, normal: Vector3, color: Color) -> SceneObject {
        SceneObject {
            color,
            kind: SceneObjectKind::Plane(center, normal),
        }
    }
}

struct Sphere;
impl Sphere {
    fn new(center: Vector3, radius: f64, color: Color) -> SceneObject {
        SceneObject {
            color,
            kind: SceneObjectKind::Sphere(center, radius),
        }
    }
}

struct SceneLight {
    center: Vector3,
}

impl SceneLight {
    fn new(center: Vector3) -> SceneLight {
        SceneLight { center }
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
    let scene = Scene::new()
        .add_object(Sphere::new(
            Vector3::new(0.0, 0.0, 1.0),
            1.0,
            Color::new(0.0, 1.0, 0.0),
        ))
        .add_object(Plane::new(
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(0.0, 0.0, 1.0).norm(),
            Color::new(1.0, 0.0, 0.0),
        ))
        .add_light(SceneLight::new(Vector3::new(50.0, -50.0, 50.0)))
        .add_camera(
            Vector3::new(5.0, 5.0, 5.0),
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(-1.0, -1.0, 2.0).norm(),
            1.0,
            2.0,
            1.5,
            640,
            480,
        );

    scene.trace_to("out.png");
}
