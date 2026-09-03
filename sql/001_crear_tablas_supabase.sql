-- =============================================================
-- TESIS DCE - ESQUEMA DE RESPUESTAS EN SUPABASE / POSTGRESQL
-- =============================================================
-- Ejecutar una vez en Supabase -> SQL Editor.
-- Este script crea tres tablas equivalentes a los tres CSV
-- principales que genera actualmente la app.
-- =============================================================

begin;

-- Usamos un esquema propio para que las respuestas no queden expuestas
-- por defecto mediante la Data API de Supabase.
create schema if not exists tesis_dce;

create table if not exists tesis_dce.dce_participantes (
  id_sesion text primary key,
  consentimiento_aceptado boolean not null,
  consentimiento_fecha timestamptz,
  inicio timestamptz not null,
  fin timestamptz not null,
  duracion_segundos double precision,
  bloque_dce smallint not null check (bloque_dce between 1 and 4),
  orden_categorias text not null,
  categoria_1 text,
  categoria_2 text,
  categoria_3 text,
  cumple_actividad boolean,
  cumple_ventas boolean,
  cumple_comuna boolean,
  cumple_cargo boolean,
  elegible_muestra boolean,
  n_elecciones_dce smallint not null default 0,
  completada boolean not null default true,

  -- Caracterización
  cargo text,
  conocimiento_ia text,
  actividad_principal text,
  ventas_anuales text,
  trabajadores integer,
  antiguedad integer,
  comuna text,
  nivel_adopcion_ia text,
  sistemas_digitales text,
  n_establecimientos integer,

  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

create table if not exists tesis_dce.dce_elecciones (
  id_sesion text not null references tesis_dce.dce_participantes(id_sesion) on delete cascade,
  bloque_dce smallint not null check (bloque_dce between 1 and 4),
  categoria_posicion smallint not null check (categoria_posicion between 1 and 3),
  categoria_id text not null,
  categoria_nombre text not null,
  tarea_posicion smallint not null check (tarea_posicion between 1 and 4),
  tarea_diseno smallint not null check (tarea_diseno between 1 and 4),
  tarea_id text not null,
  lados_intercambiados boolean not null,
  perfil_A_id text not null,
  perfil_B_id text not null,
  n_diferencias integer not null,
  eleccion text not null check (eleccion in ('A', 'B', 'ninguna')),
  perfil_elegido_id text,
  alternativa_diseno_elegida text not null check (alternativa_diseno_elegida in ('A', 'B', 'ninguna')),
  eligio_A boolean not null,
  eligio_B boolean not null,
  eligio_ninguna boolean not null,
  created_at timestamptz not null default now(),
  primary key (id_sesion, categoria_id, tarea_posicion)
);

create table if not exists tesis_dce.dce_atributos_presentados (
  id_sesion text not null references tesis_dce.dce_participantes(id_sesion) on delete cascade,
  bloque_dce smallint not null check (bloque_dce between 1 and 4),
  categoria_posicion smallint not null check (categoria_posicion between 1 and 3),
  categoria_id text not null,
  tarea_posicion smallint not null check (tarea_posicion between 1 and 4),
  tarea_diseno smallint not null check (tarea_diseno between 1 and 4),
  tarea_id text not null,
  alternativa text not null check (alternativa in ('A', 'B')),
  perfil_id text not null,
  alternativa_elegida boolean not null,
  eleccion_tarea text not null check (eleccion_tarea in ('A', 'B', 'ninguna')),
  atributo_orden integer not null,
  atributo_id text not null,
  atributo_nombre text not null,
  nivel_id text not null,
  nivel_etiqueta text not null,
  nivel_orden integer,
  precio_clp bigint,
  created_at timestamptz not null default now(),
  primary key (id_sesion, categoria_id, tarea_posicion, alternativa, atributo_id)
);

create index if not exists idx_dce_participantes_fin
  on tesis_dce.dce_participantes(fin);

create index if not exists idx_dce_participantes_bloque
  on tesis_dce.dce_participantes(bloque_dce);

create index if not exists idx_dce_elecciones_categoria
  on tesis_dce.dce_elecciones(categoria_id);

create index if not exists idx_dce_atributos_categoria
  on tesis_dce.dce_atributos_presentados(categoria_id);

commit;
