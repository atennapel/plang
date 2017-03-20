var E = require('./exprs');

function simplifyR(e) {
  if(e.tag === E.Var)
    return e;
  if(e.tag === E.App)
    return E.app(simplify(e.left), simplify(e.right));
  if(e.tag === E.Lam)
    return E.lam(e.arg, simplify(e.body));
  if(e.tag === E.Let)
    return E.lt(e.arg, simplify(e.val), simplify(e.body));
  if(e.tag === E.Letr)
    return E.ltr(e.arg, simplify(e.val), simplify(e.body));
  if(e.tag === E.Do)
    return E.app(E.vr('_do'), simplify(e.val), E.lam(e.arg, simplify(e.body)));
  if(e.tag === E.If)
    return E.iff(simplify(e.cond), simplify(e.bodyTrue), simplify(e.bodyFalse));
  if(e.tag === E.TypeOf)
    return e;

  if(e.tag === E.Anno)
    return simplify(e.expr);

  if(e.tag === E.RecordEmpty)
    return E.vr('_unit');

  if(e.tag === E.Select)
    return E.app(E.vr('_select'), E.str(e.label));
  if(e.tag === E.Extend)
    return E.app(E.vr('_extend'), E.str(e.label));
  if(e.tag === E.Restrict)
    return E.app(E.vr('_restrict'), E.str(e.label));
  if(e.tag === E.RecordUpdate)
    return E.app(E.vr('_recordupdate'), E.str(e.label));

  if(e.tag === E.Inject)
    return E.app(E.vr('_inject'), E.str(e.label));
  if(e.tag === E.Embed)
    return E.app(E.vr('_embed'), E.str(e.label));
  if(e.tag === E.Elim)
    return E.app(E.vr('_elim'), E.str(e.label));
  if(e.tag === E.VariantUpdate)
    return E.app(E.vr('_variantupdate'), E.str(e.label));

  if(e.tag === E.Handle)
    return E.app(E.vr('_handle'), E.str(e.label));
  if(e.tag === E.HandleReturn)
    return E.vr('_handlereturn');

  if(e.tag === E.End)
    return E.vr('_end');
  if(e.tag === E.Pure)
    return E.vr('_pure');
  if(e.tag === E.Return)
    return E.vr('_return');

  if(e.tag === E.Pack)
    return E.app(E.vr('_pack'), E.str(e.label));
  if(e.tag === E.Unpack)
    return E.app(E.vr('_unpack'), E.str(e.label));

  if(e.tag === E.Perform)
    return E.app(E.vr('_perform'), E.str(e.label));

  throw new Error('Cannot simplify ' + e);
}

function simplify(e) {
  var ne = simplifyR(e);
  ne.meta.type = e.meta.type;
  return ne;
}

module.exports = {
  simplify,
};
