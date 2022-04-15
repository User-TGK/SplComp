use crate::ast::{Field, Type};

use super::*;

#[derive(Default)]
pub struct Builtin {}

impl Builtin {
    pub fn load(&self, context: &mut TypeEnv, generator: &mut VarGenerator) {
        self.load_field_ops(context, generator);
        self.load_print(context, generator);
        self.load_is_empty(context, generator);
    }

    fn load_field_ops(&self, context: &mut TypeEnv, generator: &mut VarGenerator) {
        let hd_tv = Type::Var(generator.new_var());
        let hd_ty = Type::Function(vec![Type::List(Box::new(hd_tv.clone()))], Box::new(hd_tv));
        let hd_ty_generalized = context.generalize(&hd_ty);

        let tl_tv = Type::Var(generator.new_var());
        let tl_ty = Type::Function(
            vec![Type::List(Box::new(tl_tv.clone()))],
            Box::new(Type::List(Box::new(tl_tv))),
        );
        let tk_ty_generalized = context.generalize(&tl_ty);

        let (fst_tv1, fst_tv2): (Type, Type) =
            (generator.new_var().into(), generator.new_var().into());
        let fst_ty = Type::Function(
            vec![Type::Tuple(Box::new(fst_tv1.clone()), Box::new(fst_tv2))],
            Box::new(fst_tv1),
        );
        let fst_ty_generalized = context.generalize(&fst_ty);

        let (snd_tv1, snd_tv2): (Type, Type) =
            (generator.new_var().into(), generator.new_var().into());
        let snd_ty = Type::Function(
            vec![Type::Tuple(Box::new(snd_tv1), Box::new(snd_tv2.clone()))],
            Box::new(snd_tv2),
        );
        let snd_ty_generalized = context.generalize(&snd_ty);

        context.insert(Field::Hd.builtin_identifier(), hd_ty_generalized);
        context.insert(Field::Tl.builtin_identifier(), tk_ty_generalized);
        context.insert(Field::Fst.builtin_identifier(), fst_ty_generalized);
        context.insert(Field::Snd.builtin_identifier(), snd_ty_generalized);
    }

    fn load_print(&self, context: &mut TypeEnv, generator: &mut VarGenerator) {
        let p_tv = Type::Var(generator.new_var());
        let p_ty = Type::Function(vec![p_tv], Box::new(Type::Void));
        let p_ty_generalized = context.generalize(&p_ty);

        context.insert(String::from("print").into(), p_ty_generalized);
    }

    fn load_is_empty(&self, context: &mut TypeEnv, generator: &mut VarGenerator) {
        let ie_tv = Type::Var(generator.new_var());
        let ie_ty = Type::Function(vec![Type::List(Box::new(ie_tv))], Box::new(Type::Bool));
        let ie_ty_generalized = context.generalize(&ie_ty);

        context.insert(String::from("isEmpty").into(), ie_ty_generalized);
    }
}
