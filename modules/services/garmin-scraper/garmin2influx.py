import os
import sys
from datetime import date, datetime, timedelta, timezone
from garminconnect import (
    Garmin,
    GarminConnectConnectionError,
    GarminConnectTooManyRequestsError,
    GarminConnectAuthenticationError,
    )
from influxdb_client import InfluxDBClient, Point, WritePrecision
from influxdb_client.client.write_api import SYNCHRONOUS

email = os.getenv('EMAIL')
password = os.getenv('PASSWORD')
token = os.getenv('TOKEN')
org = 'default'
bucket = 'default'


def hr2point(time, val):
    return Point("health") \
      .field("heart_rate", val) \
      .time(
          datetime.fromtimestamp(time / 1000, timezone.utc),
          WritePrecision.S
      )


def hr_for_date(api, date_to_fetch):
    return api.get_heart_rates(date_to_fetch.isoformat())['heartRateValues']


date_to_fetch = date.today().isoformat()
if len(sys.argv) > 1:
    date_to_fetch = sys.argv[1]

date_to_fetch = date.fromisoformat(date_to_fetch)

try:
    api = Garmin(email, password)
    api.login()
    points = list(map(
        lambda p: hr2point(*p),
        hr_for_date(api, date_to_fetch - timedelta(days=1))
        ))
    points += list(map(
        lambda p: hr2point(*p),
        hr_for_date(api, date_to_fetch)
        ))
    with InfluxDBClient(
        url="https://stats.chvp.be:8086",
        token=token,
        org=org
    ) as client:
        write_api = client.write_api(write_options=SYNCHRONOUS)
        write_api.write(bucket, org, points)
except (
    GarminConnectConnectionError,
    GarminConnectAuthenticationError,
    GarminConnectTooManyRequestsError,
) as err:
    print(
        f'Error occured during Garmin Connect communication: {err}',
        file=sys.stderr
        )
    sys.exit(1)
