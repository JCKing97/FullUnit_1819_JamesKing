from flask_wtf import FlaskForm
from wtforms import SelectField, SubmitField, IntegerField
from wtforms.validators import number_range


class MatchSelectPlayersForm(FlaskForm):
    """The form used to select 2 players for one match"""
    strats_field1 = SelectField('Player 1', coerce=str)
    strats_field2 = SelectField('Player 2', coerce=str)
    rounds = IntegerField('How many rounds to play', validators=[number_range(2, 50)])
    submit = SubmitField('Run match')

