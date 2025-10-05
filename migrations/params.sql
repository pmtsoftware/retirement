ALTER TABLE simulations ADD param_id BIGINT REFERENCES simulation_params (id);
