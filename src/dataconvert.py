import pandas as pd
import pytz

pacific_tz = pytz.timezone('America/Los_Angeles')

df = pd.read_json(r"data\raw\yuuki_location-history.json")
df["tz_info"]=df["startTime"].str[23:]
df["endTime"] = pd.to_datetime(df["endTime"].str[:23])
df["startTime"] = pd.to_datetime(df["startTime"].str[:23])
df.set_index(["endTime","startTime","tz_info"],inplace=True)

# timelinePath
tp = df["timelinePath"].dropna()
tp = pd.json_normalize(tp.explode()).set_index(tp.explode().index)
tp[["path_lat","path_lon"]] = tp["point"].str[4:].str.split(",",expand=True).astype(float)
tp = tp.drop(["point"],axis=1).reset_index()
tp["durationMinutesOffsetFromStartTime"]=tp["durationMinutesOffsetFromStartTime"].astype(int)
tp["pointTime"] = (
    tp["startTime"].dt.tz_localize('UTC').dt.tz_convert(pacific_tz)
    + pd.to_timedelta(tp["durationMinutesOffsetFromStartTime"], unit='minute')
) # Convert to local time

tp["prevTime"]=tp["pointTime"].shift() # Calculate time difference

# timelineMemory
tm = df["timelineMemory"].dropna()
tm = pd.json_normalize(tm).set_index(tm.index).explode("destinations")
tm["destinations"]=tm["destinations"].map(lambda x: x["identifier"])
tm["distanceFromOriginKms"]=tm["distanceFromOriginKms"].astype(int)
tm.reset_index(inplace=True)

# activity
ac = df["activity"].dropna()
ac = pd.json_normalize(ac).set_index(ac.index)
ac[["start_lat","start_lon"]]=ac["start"].str[4:].str.split(",",expand=True).astype(float)
ac[["end_lat","end_lon"]]=ac["end"].str[4:].str.split(",",expand=True).astype(float)
ac.drop(["start","end"],axis=1,inplace=True)
ac["distanceMeters"]=ac["distanceMeters"].astype(float)
ac.reset_index(inplace=True)

# visit
vt = df["visit"].dropna()
vt = pd.json_normalize(vt).set_index(vt.index)
vt[["place_lat","place_lon"]]=vt["topCandidate.placeLocation"].str[4:].str.split(",",expand=True).astype(float)
vt.drop(["topCandidate.placeLocation"],axis=1,inplace=True)
vt["hierarchyLevel"]=vt["hierarchyLevel"].astype(int)
vt["topCandidate.probability"]=vt["topCandidate.probability"].astype(float)
vt.reset_index(inplace=True)

# timelinePath
tp.to_csv(r'data\converted\timelinePath.csv', index=False)

# timelineMemory
tm.to_csv(r'data\converted\timelineMemory.csv', index=False)

# activity
ac.to_csv(r'data\converted\activity.csv', index=False)

# visit
vt.to_csv(r'data\converted\visit.csv', index=False)

