-- migrate:up
CREATE TABLE "todo" (
    "uid" serial NOT NULL,
    "title" text NOT NULL,
    "done" boolean NOT NULL,
    CONSTRAINT "todo_pk" PRIMARY KEY ("uid")
);

CREATE OR REPLACE FUNCTION notify_event() RETURNS TRIGGER AS $$

    DECLARE 
        data json;
        notification json;
    
    BEGIN
    
        -- Convert the old or new row to JSON, based on the kind of action.
        -- Action = DELETE?             -> OLD row
        -- Action = INSERT or UPDATE?   -> NEW row
        IF (TG_OP = 'DELETE') THEN
            data = row_to_json(OLD);
        ELSE
            data = row_to_json(NEW);
        END IF;
        
        -- Contruct the notification as a JSON string.
        notification = json_build_object(
                          'table',TG_TABLE_NAME,
                          'action', TG_OP,
                          'data', data);
        
                        
        -- Execute pg_notify(channel, notification)
        PERFORM pg_notify('events',notification::text);
        
        -- Result is ignored since this is an AFTER trigger
        RETURN NULL; 
    END;
    
$$ LANGUAGE plpgsql;

CREATE TRIGGER todo_notify_event
AFTER INSERT OR UPDATE OR DELETE ON todo
  FOR EACH ROW EXECUTE PROCEDURE notify_event();

-- migrate:down
DROP TRIGGER "todo_notify_event" ON "todo";

DROP FUNCTION "notify_event";

DROP TABLE "todo";

