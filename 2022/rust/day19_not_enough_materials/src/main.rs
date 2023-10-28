use std::cmp::Ordering;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
enum Material {
    Ore = 0,
    Clay,
    Obsidian,
    Geode,
}

// Comment here

#[derive(Debug, Clone, Eq, PartialEq)]
struct Blueprint {
    minutes: usize,
    blueprint_id: usize,
    robot_build_index: usize,
    robot_cost: Vec<(usize, Option<(Material, usize)>)>,
    material_count: [usize; 4],
    robots_count: [usize; 4],
    new_robots_count: [usize; 4],
}

const MATERIALS: [Material; 4] = [
    Material::Ore,
    Material::Clay,
    Material::Obsidian,
    Material::Geode,
];

fn get_material_index(material: &Material) -> usize {
    match material {
        Material::Ore => 0,
        Material::Clay => 1,
        Material::Obsidian => 2,
        Material::Geode => 3,
    }
}

fn get_material(material: &str) -> Material {
    match material {
        "ore" => Material::Ore,
        "clay" => Material::Clay,
        "obsidian" => Material::Obsidian,
        "geode" => Material::Geode,
        _ => panic!("Unexpected material!"),
    }
}

impl Blueprint {
    fn new(line: &str) -> Self {
        let mut line_iter = line.split(':');
        let blueprint_id = line_iter
            .next()
            .unwrap()
            .trim()
            .split(' ')
            .into_iter()
            .skip(1)
            .next()
            .unwrap()
            .parse::<usize>()
            .unwrap();

        let costs: Vec<&str> = line_iter
            .next()
            .unwrap()
            .trim()
            .split('.')
            .filter(|s| !s.is_empty())
            .collect();

        let robot_cost = costs
            .into_iter()
            .map(|cost| {
                let tokens: Vec<&str> = cost.trim().split(' ').collect();

                let material = get_material(tokens[1]);

                match material {
                    Material::Ore => (tokens[4].parse::<usize>().unwrap(), None),
                    Material::Clay => (tokens[4].parse::<usize>().unwrap(), None),
                    Material::Obsidian => (
                        tokens[4].parse::<usize>().unwrap(),
                        Some((get_material(tokens[8]), tokens[7].parse::<usize>().unwrap())),
                    ),
                    Material::Geode => (
                        tokens[4].parse::<usize>().unwrap(),
                        Some((get_material(tokens[8]), tokens[7].parse::<usize>().unwrap())),
                    ),
                }
            })
            .collect();

        Self {
            minutes: 0,
            robot_cost,
            blueprint_id,
            robot_build_index: 0,
            material_count: [0, 0, 0, 0],
            robots_count: [1, 0, 0, 0],
            new_robots_count: [0, 0, 0, 0],
        }
    }

    fn compare_count(count1: [usize; 4], count2: [usize; 4]) -> Ordering {
        let [ore1, clay1, obsidian1, geode1] = count1;
        let [ore2, clay2, obsidian2, geode2] = count2;

        match geode1.cmp(&geode2) {
            Ordering::Equal => match obsidian1.cmp(&obsidian2) {
                Ordering::Equal => match clay1.cmp(&clay2) {
                    Ordering::Equal => ore1.cmp(&ore2),
                    ord => ord,
                },
                ord => ord,
            },
            ord => ord,
        }
    }

    fn get_quality_level(self) -> usize {
        let blueprint_id = self.blueprint_id;

        let mut pending: Vec<Blueprint> = vec![];
        pending.insert(self.minutes, self);

        let mut max_material_counts = [([0_usize, 0, 0, 0], [1_usize, 0, 0, 0]); 25];
        // let mut max_robot_counts = [[1_usize, 0, 0, 0]; 25];

        let mut max_geode_count = 0;
        let mut min_minutes = 24;

        while let Some(mut blueprint) = pending.pop() {
            if blueprint.minutes == 23 {
                for blueprint in blueprint.build_robots() {
                    let geode_count =
                        blueprint.material_count[get_material_index(&Material::Geode)];
                    if geode_count > 0 {
                        if geode_count > max_geode_count {
                            max_geode_count = geode_count;
                            if blueprint.minutes < min_minutes {
                                min_minutes = blueprint.minutes;
                            }
                        } else if geode_count == max_geode_count {
                            if blueprint.minutes < min_minutes {
                                min_minutes = blueprint.minutes;
                            }
                        }
                    }
                }
            } else {
                let robots = blueprint.build_robots();

                for blueprint in robots {
                    match Self::compare_count(
                        blueprint.material_count,
                        max_material_counts[blueprint.minutes].0,
                    ) {
                        Ordering::Less => {
                            match Self::compare_count(
                                blueprint.robots_count,
                                max_material_counts[blueprint.minutes].1,
                            ) {
                                Ordering::Less => continue,
                                Ordering::Equal => continue,
                                Ordering::Greater => {
                                    pending.push(blueprint);
                                }
                            }
                        }
                        Ordering::Greater => {
                            max_material_counts[blueprint.minutes] =
                                (blueprint.material_count, blueprint.robots_count);
                            // max_robot_counts[blueprint.minutes] = blueprint.robots_count;
                            pending.push(blueprint);
                        }
                        Ordering::Equal => {
                            match Self::compare_count(
                                blueprint.robots_count,
                                max_material_counts[blueprint.minutes].1,
                            ) {
                                Ordering::Less => continue,
                                Ordering::Equal => pending.push(blueprint),
                                Ordering::Greater => {
                                    // max_material_counts[blueprint.minutes].1 =
                                    //     blueprint.robots_count;
                                    pending.push(blueprint);
                                }
                            }
                        }
                    }
                }
            }
        }

        blueprint_id * max_geode_count
    }

    fn produce_raw_materials(&mut self) {
        for (index, _) in MATERIALS.into_iter().enumerate() {
            self.material_count[index] += self.robots_count[index];
        }

        for index in 0..4 {
            self.robots_count[index] += self.new_robots_count[index];
            self.new_robots_count[index] = 0;
        }
    }

    fn pay_cost_for_robot(&mut self, robot_kind: &Material) {
        let index = get_material_index(robot_kind);
        match &self.robot_cost[index] {
            (ore_cost, None) => self.material_count[0] -= ore_cost,
            (ore_cost, Some((material, material_cost))) => {
                let material_index = get_material_index(material);
                self.material_count[0] -= ore_cost;
                self.material_count[material_index] -= material_cost;
            }
        }
        self.new_robots_count[get_material_index(robot_kind)] += 1;
    }

    fn cost_sufficient_to_build_robot(&self, robot_kind: &Material) -> bool {
        match robot_kind {
            Material::Ore => self.material_count[0] >= self.robot_cost[0].0,
            Material::Clay => self.material_count[0] >= self.robot_cost[1].0,
            Material::Obsidian => {
                self.material_count[0] >= self.robot_cost[2].0
                    && self.material_count[1] >= self.robot_cost[2].1.clone().unwrap().1
            }
            Material::Geode => {
                self.material_count[0] >= self.robot_cost[3].0
                    && self.material_count[2] >= self.robot_cost[3].1.clone().unwrap().1
            }
        }
    }

    fn build_robot_variants(&self, robot_kind: &Material) -> Vec<Blueprint> {
        let mut blueprints = vec![];

        if self.cost_sufficient_to_build_robot(robot_kind) {
            let mut blueprint = self.clone();
            loop {
                let mut blueprint1 = blueprint.clone();
                blueprint1.pay_cost_for_robot(robot_kind);
                blueprints.push(blueprint1.clone());

                if !blueprint1.cost_sufficient_to_build_robot(robot_kind) {
                    break;
                } else {
                    blueprint = blueprint1;
                }
            }
        }
        blueprints
    }

    fn build_robots(&mut self) -> Vec<Blueprint> {
        let mut blueprints = vec![];
        self.minutes += 1;

        let mut blueprint = self.clone();
        blueprint.produce_raw_materials();
        blueprints.push(blueprint);

        for material in MATERIALS {
            for blueprint in self.build_robot_variants(&material) {
                let mut blueprint = blueprint.clone();
                blueprint.produce_raw_materials();
                blueprints.push(blueprint);
            }
        }

        blueprints
    }
}

fn main() {
    // let lines = std::io::stdin().lines();

    let lines = "Blueprint 1:  Each ore robot costs 4 ore.  Each clay robot costs 2 ore.  Each obsidian robot costs 3 ore and 14 clay.  Each geode robot costs 2 ore and 7 obsidian.
    Blueprint 2:  Each ore robot costs 2 ore.  Each clay robot costs 3 ore.  Each obsidian robot costs 3 ore and 8 clay.  Each geode robot costs 3 ore and 12 obsidian".split('\n');

    let mut total = 0;

    for line in lines {
        let blueprint = Blueprint::new(line);
        let quality_level = blueprint.get_quality_level();
        total += quality_level;
        println!("{quality_level:?} {total:?}");
    }
}
