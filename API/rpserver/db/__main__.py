import os
from alembic.config import CommandLine, Config
from pathlib import Path
from flask import current_app


DEFAULT_PG_URL = current_app.config.get('DATABASE_PG_URL')
PROJECT_PATH = Path(__file__).parent.parent.resolve()


def main():
    alembic = CommandLine()
    alembic.parser.add_argument(
        '--pg-url', default=os.getenv('ANALYZER_PG_URL', DEFAULT_PG_URL),
        help='Database URL [env var: ANALYZER_PG_URL]'
    )
    options = alembic.parser.parse_args()

    # Создаем объект конфигурации Alembic
    config = Config(file_=options.config, ini_section=options.name,
                    cmd_opts=options)

    # Меняем значение sqlalchemy.url из конфига Alembic
    config.set_main_option('sqlalchemy.url', options.pg_url)

    # Запускаем команду alembic
    exit(alembic.run_cmd(config, options))


if __name__ == '__main__':
    main()