from flask_wtf import FlaskForm
from wtforms import SelectField, SubmitField, StringField


class GameSelectAgentsForm(FlaskForm):
    strats_field1 = SelectField('Agent 1', coerce=str)
    strats_field2 = SelectField('Agent 2', coerce=str)
    submit = SubmitField('Run game')

