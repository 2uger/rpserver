"""empty message

Revision ID: 62f299aa9238
Revises: 
Create Date: 2020-04-17 19:05:59.184469

"""
from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision = '62f299aa9238'
down_revision = None
branch_labels = None
depends_on = None


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.create_table('coordinate',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('longitude', sa.Integer(), nullable=False),
    sa.Column('latitude', sa.Integer(), nullable=False),
    sa.Column('rider_name', sa.String(length=100), nullable=False),
    sa.PrimaryKeyConstraint('id'),
    sa.UniqueConstraint('rider_name')
    )
    op.create_table('rider',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('name', sa.String(length=100), nullable=False),
    sa.Column('surname', sa.String(length=100), nullable=False),
    sa.Column('age', sa.Integer(), nullable=False),
    sa.Column('login_email', sa.String(length=100), nullable=False),
    sa.Column('password', sa.String(length=100), nullable=False),
    sa.Column('hometown', sa.String(length=100), nullable=False),
    sa.Column('profile_image', sa.String(length=100), nullable=True),
    sa.Column('remember_me', sa.Boolean(), nullable=True),
    sa.PrimaryKeyConstraint('id')
    )
    op.create_table('spot',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('name', sa.String(length=50), nullable=False),
    sa.Column('profile_image', sa.String(length=50), nullable=False),
    sa.Column('location', sa.String(length=100), nullable=False),
    sa.Column('notes', sa.String(length=200), nullable=False),
    sa.PrimaryKeyConstraint('id'),
    sa.UniqueConstraint('location'),
    sa.UniqueConstraint('notes'),
    sa.UniqueConstraint('profile_image')
    )
    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.drop_table('spot')
    op.drop_table('rider')
    op.drop_table('coordinate')
    # ### end Alembic commands ###
