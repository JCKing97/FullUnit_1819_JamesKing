from flask_wtf import FlaskForm
from wtforms import SelectField, SubmitField


class GameSelectAgentsForm(FlaskForm):
    """The form used to select 2 agents for one game"""
    strats_field1 = SelectField('Agent 1', coerce=str)
    strats_field2 = SelectField('Agent 2', coerce=str)
    submit = SubmitField('Run game')

